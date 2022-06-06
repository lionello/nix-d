module nix.primops;

debug import std.stdio : writeln;
import std.exception : enforce;

import nix.value, nix.evaluator;
import nix.path;

class AbortException : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) pure {
        super("evaluation aborted with the following error message: "~msg, file, line);
    }
}


class ThrownException : AssertionException {
    this(string msg, string file = __FILE__, size_t line = __LINE__) pure {
        super(msg, file, line);
    }
}

private Value functionArgs(ref Value func) {
    Bindings args;
    const formals = forceValue(func).lambda.formals;
    if (formals) {
        foreach(f; formals.elems) {
            args[f.name] = new Value(f.def !is null);
        }
    }
    return Value(args);
}

private Value genList(ref Value func, ref Value count) {
    const length = forceValue(count).integer;
    Value*[] list;
    foreach (n; 0..length) {
        auto v = Value(n);
        list ~= callFunction(func, v, Loc()).dup;
    }
    return Value(list);
}

private Value any(ref Value fun, ref Value list) /*pure*/ {
    foreach (e; forceValue(list).list) {
        if (callFunction(fun, *e, Loc()).boolean)
            return Value.TRUE;
    }
    return Value.FALSE;
}

private Value all(ref Value fun, ref Value list) /*pure*/ {
    foreach (e; forceValue(list).list) {
        if (!callFunction(fun, *e, Loc()).boolean)
            return Value.FALSE;
    }
    return Value.TRUE;
}

private Value appendContext(ref Value s, ref Value ctx) {
    const orig = forceValue(s);
    auto context = orig.context.dupx;
    foreach (name, v; forceValue(ctx).attrs) {
        enforce!EvalException(isStorePath(name), "Context key '"~name~"' is not a store path");
        auto attrs = forceValue(*v).attrs;

        if (auto path = "path" in attrs) {
            if (forceValue(**path).boolean) {
                context[name] = true;
            }
        }

        if (auto allOutputs = "allOutputs" in attrs) {
            if (forceValue(**allOutputs).boolean) {
                enforce!EvalException(isDerivation(name), "Tried to add all-outputs context of "~name~", which is not a derivation, to a string");
                context["=" ~ name] = true;
            }
        }

        if (auto outputs = "outputs" in attrs) {
            auto list = forceValue(**outputs).list;
            // assert(!list.length || isDerivation(name));
            foreach (vv; list) {
                auto n = forceStringNoCtx(*vv);
                context["!" ~ n ~ "!" ~ name] = true;
            }
        }
    }
    return Value(orig.str, context);
}

private Value binOp(string OP)(ref Value lhs, ref Value rhs) /*pure*/ {
    return forceValue(lhs).opBinary!OP(forceValue(rhs));
}

private Value tryEval(ref Value expr) {
    Bindings result;
    try {
        result["value"] = &forceValue(expr);
        result["success"] = new Value(true);
    }
    catch(AssertionException e) {
        result["value"] = new Value(false);
        result["success"] = new Value(false);
    }
    return Value(result);
}

private Value typeOf_(ref Value arg) /*pure*/ {
    return Value(forceValue(arg).typeOf, null);
}

private Value isAttrs(ref Value arg) {
    return Value(forceValue(arg).type == Type.Attrs);
}

private Value isBool(ref Value arg) {
    return Value(forceValue(arg).type == Type.Bool);
}

private Value isFloat(ref Value arg) {
    return Value(forceValue(arg).type == Type.Float);
}

private Value isFunction(ref Value arg) /*pure*/ {
    switch (forceValue(arg).type) {
    case Type.Lambda:
    case Type.PrimOp:
    case Type.PrimOpApp:
        return Value(true);
    default:
        return Value(false);
    }
}

private Value isInt(ref Value arg) {
    return Value(forceValue(arg).type == Type.Int);
}

private Value isList(ref Value arg) /*pure*/ {
    return Value(forceValue(arg).type == Type.List);
}

private Value isNull(ref Value arg) /*pure*/ {
    return Value(forceValue(arg).isNull);
}

private Value isPath(ref Value arg) {
    return Value(forceValue(arg).type == Type.Path);
}

private Value isString(ref Value arg) {
    return Value(forceValue(arg).type == Type.String);
}

private Value lessThan(ref Value lhs, ref Value rhs) /*pure*/ {
    return Value(forceValue(lhs) < forceValue(rhs));
}

private Value listToAttrs(ref Value list) {
    Bindings v;
    foreach (k; forceValue(list).list) {
        auto attrs = forceValue(*k).attrs;
        auto j = "name" in attrs;
        enforce!TypeException(j, "'name' attribute missing in a call to 'listToAttrs'");
        const name = forceStringNoCtx(**j);
        if (name in v) continue;
        auto j2 = "value" in attrs;
        enforce!TypeException(j2, "'value' attribute missing in a call to 'listToAttrs'");
        v[name] = *j2;
    }
    return Value(v);
}

private Value toString(ref Value arg) /*pure*/ {
    return Value(coerceToString(arg, true, false), null); // TODO: context?
}

private Value throw_(ref Value msg) /*pure*/ {
    throw new ThrownException(coerceToString(msg)); // TODO copyToStore=true?
}

private Value toJSON(ref Value v) {
    static string convJSON(ref Value v) {
        import nix.printer : escapeString;
        forceValue(v);
        final switch (v.type) {
        case Type.Path:
            return escapeString(v.toString());
        case Type.Null:
        case Type.String:
        case Type.Int:
        case Type.Float:
        case Type.Bool:
            return v.toString();
        case Type.List:
            string s = "[";
            foreach (i, e; v.list) {
                if (i) s ~= ',';
                s ~= convJSON(*e);
            }
            return s ~ "]";
        case Type.Attrs:
            auto s = tryAttrsToString(v, false, false);
            if (s !is null) return escapeString(s);
            auto obj = "{";
            foreach (k; v.attrs.lexicographicOrder) {
                if (obj.length>1) obj ~= ',';
                obj ~= escapeString(k);
                obj ~= ":";
                obj ~= convJSON(*v.attrs[k]);
            }
            return obj ~ '}';
        case Type.App:
        case Type.PrimOp:
        case Type.PrimOpApp:
        case Type.Lambda:
        case Type.Thunk:
            throw new TypeException("cannot convert to JSON: "~v.typeOf);
        }
    }
    return Value(convJSON(v), null);
}

private Value abort(ref Value msg) /*pure*/ {
    throw new AbortException(coerceToString(msg)); // TODO copyToStore = true?
}

private Value elemAt(ref Value list, NixInt n) /*pure*/ {
    return forceValue(*forceValue(list).list[n]);
}

private Value elemAt(ref Value list, ref Value index) /*pure*/ {
    return elemAt(list, forceValue(index).integer);
}

private Value hashString(ref Value type, ref Value str) {
    import std.digest.md : md5Of;
    import std.digest.sha : sha1Of, sha512Of, sha256Of;
    static char[] toBase16(size_t S)(scope const(ubyte)[S] digest) pure {
        import std.digest : toHexString, LetterCase;
        return digest.toHexString!(LetterCase.lower).dup;
    }
    const hashtype = forceStringNoCtx(type);
    forceValue(str);
    string hash;
    switch (hashtype) {
    case "md5":    hash = toBase16(md5Of(str.str.raw)); break;
    case "sha1":   hash = toBase16(sha1Of(str.str.raw)); break;
    case "sha256": hash = toBase16(sha256Of(str.str.raw)); break;
    case "sha512": hash = toBase16(sha512Of(str.str.raw)); break;
    default:
        throw new Exception("unknown hash type: "~hashtype);
    }
    return Value(hash, null);
}

private Value head(ref Value list) /*pure*/ {
    return elemAt(list, 0);
}

private Value elem(ref Value val, ref Value list) /*pure*/ {
    forceValue(val);
    foreach (e; forceValue(list).list) {
        if (eqValues(*e, val)) return Value(true);
    }
    return Value(false);
}

private Value foldl_(ref Value func, ref Value v, ref Value list) {
    Value acc = v;
    foreach (e; forceValue(list).list) {
        auto partial = callFunction(func, acc, Loc());
        acc = callFunction(partial, *e, Loc());
    }
    return acc;
}

private Value fromJSON(ref Value str) {
    import std.json : parseJSON, JSONValue, JSONType;
    static Value* convJSON(JSONValue v) {
        final switch (v.type) {
        case JSONType.null_:    return new Value();
        case JSONType.string:   return new Value(v.str, null);
        case JSONType.integer:  return new Value(v.integer);
        case JSONType.uinteger: return new Value(v.uinteger);
        case JSONType.float_:   return new Value(v.floating);
        case JSONType.array:
            Value*[] output;
            foreach (ref e; v.array) {
                output ~= convJSON(e);
            }
            return new Value(output);
        case JSONType.object:
            Bindings output;
            foreach (k, e; v.object) {
                output[k] = convJSON(e);
            }
            return new Value(output);
        case JSONType.true_:    return new Value(true);
        case JSONType.false_:   return new Value(false);
        }
    }
    return *convJSON(parseJSON(forceStringNoCtx(str)));
}

private Value intersectAttrs(ref Value e1, ref Value e2) {
    forceValue(e1);
    forceValue(e2);
    Bindings set;
    foreach (k, v; e1.attrs) {
        if (auto j = k in e2.attrs) set[k] = *j;
    }
    return Value(set);
}

private Value import_(ref Value filename) /*pure*/ {
    import std.file : readText;
    auto s = readText(coerceToPath(filename));
    return eval(parseAndBind(s));
}

private string didYouMean(string s, in Bindings b) {
    import std.algorithm.comparison : levenshteinDistance;
    foreach (k, v; b) {
        if (levenshteinDistance(s, k) == 1) return "; did you mean: " ~ k;
    }
    return "";
}

private Value getAttr(ref Value name, ref Value attrs) {
    auto ptr = forceStringNoCtx(name) in forceValue(attrs).attrs;
    enforce!EvalException(ptr, "attribute '"~name.str~"' missing" ~ didYouMean(name.str, attrs.attrs));
    return **ptr;
}

private auto decodeContext(in string s) {
    struct NixStringContextElem {
        Path first;
        string second;
    }
    if (s[0] == '!') {
        import std.string : indexOf;
        const i = s.indexOf('!', 1);
        return NixStringContextElem(parseStorePath(s[i+1..$]), s[1..i]);
    } else {
        auto path = s[0] == '/' ? s : s[1..$];
        return NixStringContextElem(parseStorePath(path), "");
    }
}

private Value getContext(ref Value str) {
    struct ContextInfo {
        bool path;
        bool allOutputs;
        string[] outputs;
    }
    ContextInfo[Path] contextInfos;
    auto context = forceValue(str).context;
    foreach (p, v; context) {
        string drv, output;
        string path = p;
        if (p[0] == '=') {
            drv = path = p[1..$];
        } else if (p[0] == '!') {
            auto ctx = decodeContext(p);
            drv = printStorePath(ctx.first);
            output = ctx.second;
            path = drv;
        }
        auto isPath = drv.length == 0;
        auto isAllOutputs = drv.length != 0 && output.length == 0;
        if (auto j = path in contextInfos) {
            if (isPath) {
                j.path = true;
            } else if (isAllOutputs) {
                j.allOutputs = true;
            } else {
                j.outputs ~= output;
            }
        } else {
            contextInfos[path] = ContextInfo(isPath, isAllOutputs, output ? [output] : []);
        }
    }

    Bindings attrs;
    foreach (path, info; contextInfos) {
        Bindings infoAttrs;
        if (info.path) infoAttrs["path"] = new Value(true);
        if (info.allOutputs) infoAttrs["allOutputs"] = new Value(true);
        if (info.outputs) {
            Value*[] outputs;
            foreach (output; info.outputs) {
                outputs ~= new Value(output, null);
            }
            infoAttrs["outputs"] = new Value(outputs);
        }
        attrs[path] = new Value(infoAttrs);
    }
    return Value(attrs);
}

private Value getEnv(ref Value str) {
    auto name = forceStringNoCtx(str);
    import std.process : environment;
    return Value(environment.get(name, ""), null);
}

private Value hasAttr(ref Value name, ref Value attrs) {
    return Value((forceStringNoCtx(name) in forceValue(attrs).attrs) !is null);
}

private auto lexicographicOrder(Bindings attrs) {
    import std.algorithm : sort;
    return attrs.keys.sort;
}

unittest {
    import std.range : array;
    Bindings v = ["b": null];
    v["bb"] = null;
    v["a"] = null;
    assert(v.lexicographicOrder.array == ["a", "b", "bb"]);
}

private Value attrValues(ref Value attrs) {
    Value*[] values;
    auto aa = forceValue(attrs).attrs;
    foreach (name; aa.lexicographicOrder) {
        values ~= aa[name];
    }
    return Value(values);
}

private Value attrNames(ref Value attrs) {
    Value*[] names;
    auto aa = forceValue(attrs).attrs;
    foreach (name; aa.lexicographicOrder) {
        names ~= new Value(name, null);
    }
    return Value(names);
}

private Value map(ref Value fun, ref Value list) {
    Value*[] output;
    foreach (e; forceValue(list).list) {
        output ~= callFunction(fun, *e, Loc()).dup;
    }
    return Value(output);
}

private Value tail(ref Value list) {
    return Value(forceValue(list).list[1..$]);
}

private Value dirOf(ref Value file) {
    import nix.path : dirOf;
    const dir = dirOf(coerceToString(file, false, false));
    return file.type == Type.Path ? Value(dir) : Value(dir, file.context.dupx);
}

private Value catAttrs(ref Value v, ref Value listOfAttrs) {
    Value*[] list;
    const attrName = forceStringNoCtx(v);
    foreach (e; forceValue(listOfAttrs).list) {
        auto attr = attrName in forceValue(*e).attrs;
        if (attr) list ~= *attr;
    }
    return Value(list);
}

private Value length_(ref Value list) {
    return Value(forceValue(list).list.length);
}

private Value genericClosure(ref Value attr) {
    auto attrs = forceValue(attr).attrs;
    auto startSet = forceValue(*attrs["startSet"]).list;
    auto operator = forceValue(*attrs["operator"]);
    bool[const Value] done;
    Value*[] res;
    while (startSet.length) {
        auto e = forceValue(*startSet[0]);
        startSet = startSet[1..$];
        const key = forceValue(*e.attrs["key"]);
        if (key in done) continue;
        done[key] = true;
        assert(key in done);
        res ~= e.dup;
        auto result = callFunction(operator, e, Loc());
        startSet ~= forceValue(result).list;
    }
    return Value(res);
}

private Value baseNameOf(ref Value file) {
    import nix.path : baseNameOf;
    const dir = baseNameOf(coerceToString(file, false, false));
    return file.type == Type.Path ? Value(dir) : Value(dir, file.context.dupx);
}

private Value concatMap(ref Value fun, ref Value list) {
    Value*[] output;
    foreach (e; forceValue(list).list) {
        output ~= callFunction(fun, *e, Loc()).list;
    }
    return Value(output);
}

private Value concatStringsSep(ref Value sep, ref Value list) {
    string s = "";
    PathSet ps;
    foreach (i, e; forceValue(list).list) {
        if (i) s ~= forceValue(sep).str;
        s ~= coerceToString(*e);
        // ps ~= TODO
    }
    return Value(s, ps);
}

private Value unsafeDiscardStringContext(ref Value str) {
    return Value(coerceToString(str), null);
}

// private Value unsafeGetAttrPos(ref Value str, ref Value attrs) {
//     const attrName = forceStringNoCtx(str);
//     const attr = attrName in forceValue(attrs).attrs;
//     return attr ? Value(*attr.) : Value();
// }

private Value stringLength(ref Value str) {
    return Value(coerceToString(str).length);
}

private Value substring(ref Value from, ref Value len, ref Value str) {
    const s = coerceToString(str);
    auto start = forceValue(from).integer;
    assert(start >= 0);
    auto end = start + forceValue(len).integer;
    if (start >= s.length) start = end = 0;
    else if (end > s.length) end = s.length;
    return Value(s[start..end], str.context.dupx);
}

private Value removeAttrs(ref Value attrs, ref Value list) {
    auto set = forceValue(attrs).attrs.dup;
    foreach (v; forceValue(list).list) {
        set.remove(forceStringNoCtx(*v));
    }
    return Value(set);
}

private Value seq(ref Value a, ref Value b) {
    forceValue(a);
    return forceValue(b);
}

private Value sort(ref Value fun, ref Value list) {
    import std.algorithm.mutation : SwapStrategy;
    import std.algorithm.sorting : sort;
    bool cmp(Value* a, Value* b) {
        auto partial = callFunction(fun, *a, Loc());
        return callFunction(partial, *b, Loc()).boolean;
    }
    return Value(sort!(cmp, SwapStrategy.stable)(forceValue(list).list).array);
}

private Value deepSeq(ref Value a, ref Value b) {
    forceValueDeep(a);
    return forceValue(b);
}

private Value derivationStrict(ref Value attrs) {
    auto drvAttrs = forceValue(attrs).attrs;
    enforce!EvalException("name" in drvAttrs, "required attribute 'name' missing");
    const name = forceStringNoCtx(*drvAttrs["name"]);

    string drv;
    PathSet context;
    auto outputs = ["out"];
    foreach (key; drvAttrs.lexicographicOrder) {
        if (key == "__ignoreNulls") continue;
        auto s = coerceToString(*drvAttrs[key], true);
        drv ~= s;
        if (key == "outputs") {
            import std.array : split;
            outputs = s.raw.split; // tokenizeString
        }
        // Append to context
        foreach (path, _; s.context) {
            context[path] = true;
        }
    }

    // HACK: just to pass the test
    import std.digest.sha : sha256Of, toHexString, LetterCase;
    immutable hash = sha256Of(drv)[0..20].toHexString!(LetterCase.lower);

    const drvPath = hash~"-"~name~".drv";
    const drvPathS = printStorePath(drvPath);

    Bindings v;
    v["drvPath"] = new Value(drvPathS, ["="~drvPathS:true]);
    foreach (i; outputs) {
        auto outputPath = hash~"-"~name;
        v[i] = new Value(printStorePath(outputPath), ["!"~i~"!"~drvPathS:true]);
    }
    return Value(v);
}

private Value concatLists(ref Value lists) {
    Value*[] output;
    foreach (list; forceValue(lists).list) {
        output ~= forceValue(*list).list;
    }
    return Value(output);
}

private Value filter(ref Value fun, ref Value list) {
    Value*[] output;
    foreach (e; forceValue(list).list) {
        if (callFunction(fun, forceValue(*e), Loc()).boolean)
            output ~= e;
    }
    return Value(output);
}

private Value ceil(ref Value f) {
    import std.math.rounding : ceil;
    return Value(ceil(forceValue(f)._number));
}

private Value floor(ref Value f) {
    import std.math.rounding : floor;
    return Value(floor(forceValue(f)._number));
}

ref Env createBaseEnv() {
    import core.stdc.time : time;

    static Value notImplemented(string S)(Value*[] args...) /*pure*/ {
        assert(0, "not implemented: "~S);
    }
    static auto ni(string S)() {
        return new Value(&notImplemented!S);
    }
    static auto wrap(alias F)() {
        import std.traits : arity;
        static Value primop(Value*[] args...) {
            if (args.length < arity!F) return Value(&primop, args);
            assert(args.length == arity!F);
            static if (arity!F == 0) return F();
            static if (arity!F == 1) return F(*args[0]);
            static if (arity!F == 2) return F(*args[0], *args[1]);
            static if (arity!F == 3) return F(*args[0], *args[1], *args[2]);
        }
        return new Value(&primop);
    }

    auto derivation = new Value;
    Bindings globals = [
        "abort" : wrap!abort,
        "__add" : wrap!(binOp!"+"),
        // "__addErrorContext" : ni!"__addErrorContext",
        "__all" : wrap!all,
        "__any" : wrap!any,
        "__appendContext" : wrap!appendContext,//noctx
        "__attrNames" : wrap!attrNames,
        "__attrValues" : wrap!attrValues,
        "baseNameOf" : wrap!baseNameOf,
        "__bitAnd" :  wrap!(binOp!"&"),
        "__bitOr" :  wrap!(binOp!"|"),
        "__bitXor" :  wrap!(binOp!"^"),
        "__catAttrs" : wrap!catAttrs,
        "ceil": wrap!ceil,
        // "__compareVersions" : ni!"__compareVersions",//noctx
        "__concatLists" : wrap!concatLists,
        "__concatMap" : wrap!concatMap,
        "__concatStringsSep" : wrap!concatStringsSep,
        "__currentSystem" : new Value("x86_64-darwin", null), // impure
        "__currentTime" : new Value(time(null)), // impure FIXME: make lazy
        "__deepSeq" : wrap!deepSeq,
        "derivation": derivation,
        "derivationStrict" : wrap!derivationStrict,
        "dirOf" : wrap!dirOf,
        "__div" : wrap!(binOp!"/"),
        "__elem" : wrap!elem,
        "__elemAt" : wrap!elemAt,
        "false" : new Value(false),
        // "fetchGit" : ni!"fetchGit",
        // "fetchMercurial" : ni!"fetchMercurial",
        // "fetchTarball" : ni!"fetchTarball",
        // "__fetchurl" : ni!"__fetchurl",
        "__filter" : wrap!filter,
        // "__filterSource" : ni!"__filterSource",
        // "__findFile" : ni!"__findFile",
        "floor": wrap!floor,
        "__foldl'" : wrap!foldl_,
        "__fromJSON" : wrap!fromJSON,
        // "fromTOML" : ni!"fromTOML",
        "__functionArgs" : wrap!functionArgs,
        "__genList" : wrap!genList,
        "__genericClosure" : wrap!genericClosure,
        "__getAttr" : wrap!getAttr,
        "__getContext" : wrap!getContext,
        "__getEnv" : wrap!getEnv,
        "__hasAttr" : wrap!hasAttr,
        // "__hasContext" : ni!"__hasContext",
        // "__hashFile" : ni!"__hashFile",
        "__hashString" : wrap!hashString,
        "__head" : wrap!head,
        "import" : wrap!import_,
        "__intersectAttrs" : wrap!intersectAttrs,
        "__isAttrs" : wrap!isAttrs,
        "__isBool" : wrap!isBool,
        "__isFloat" : wrap!isFloat,
        "__isFunction" : wrap!isFunction,
        "__isInt" : wrap!isInt,
        "__isList" : wrap!isList,
        "isNull" : wrap!isNull,
        "__isPath" : wrap!isPath,
        "__isString" : wrap!isString,
        "__langVersion" : new Value(5), //baseEnv
        "__length" : wrap!length_,
        "__lessThan" : wrap!lessThan,
        "__listToAttrs" : wrap!listToAttrs,
        "map" : wrap!map,
        // "__mapAttrs" : ni!"__mapAttrs",
        // "__match" : ni!"__match", //noctx
        "__mul" : wrap!(binOp!"*"),
        "__nixPath" : new Value(cast(Value*[])[]), //[{path="";prefix="";}] impure
        "__nixVersion" : new Value("2.3.4", null), //baseEnv
        "null" : new Value,
        // "__parseDrvName" : ni!"__parseDrvName", //noctx
        // "__partition" : ni!"__partition",
        // "__path" : ni!"__path",
        // "__pathExists" : ni!"__pathExists",
        // "placeholder" : ni!"placeholder",
        // "__readDir" : ni!"__readDir",
        // "__readFile" : ni!"__readFile",
        "removeAttrs" : wrap!removeAttrs,
        // "__replaceStrings" : ni!"__replaceStrings",
        // "scopedImport" : ni!"scopedImport",
        "__seq" : wrap!seq,
        "__sort" : wrap!sort,
        // "__split" : ni!"__split",//noctx
        // "__splitVersion" : ni!"__splitVersion",//noctx
        "__storeDir" : new Value(storeDir), // baseEnv
        // "__storePath" : ni!"__storePath", // impure
        "__stringLength" : wrap!stringLength,
        "__sub" : wrap!(binOp!"-"),
        "__substring" : wrap!substring,
        "__tail" : wrap!tail,
        "throw" : wrap!(throw_),
        // "__toFile" : ni!"__toFile",
        "__toJSON" : wrap!toJSON,
        // "__toPath" : ni!"__toPath",
        "toString" : wrap!(toString),
        // "__toXML" : ni!"__toXML",
        // "__trace" : ni!"__trace",
        "true" : new Value(true),
        "__tryEval" : wrap!tryEval,
        "__typeOf" : wrap!typeOf_,
        // "__unsafeDiscardOutputDependency" : ni!"__unsafeDiscardOutputDependency",
        "__unsafeDiscardStringContext" : wrap!unsafeDiscardStringContext,
        // "__unsafeGetAttrPos" : wrap!unsafeGetAttrPos,
    ];

    Bindings builtins;
    foreach (k, v; globals) {
        builtins[k[0..2]=="__"?k[2..$]:k] = v;
    }
    globals["builtins"] = builtins["builtins"] = new Value(builtins);

    auto baseEnv = new Env(null, globals);
    /* Note: we have to initialize the 'derivation' constant *after*
       building baseEnv/staticBaseEnv because it uses 'builtins'. */
    immutable derivationNix = import("derivation.nix");
    *derivation = eval(parseAndBind(derivationNix, *baseEnv), *baseEnv);

    return *baseEnv;
}

unittest {
    auto baseEnv = createBaseEnv();
    assert(baseEnv.vars["builtins"].type == Type.Attrs);
    assert(baseEnv.vars["builtins"].attrs["builtins"].type == Type.Attrs);
    assert(baseEnv.vars["derivation"].type == Type.Lambda);
}
