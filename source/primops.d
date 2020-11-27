module nix.primops;

debug import std.stdio:writeln;

import nix.value, nix.evaluator;

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

private Value toString(ref Value arg) /*pure*/ {
    return Value(coerceToString(arg, true), null); // TODO: context?
}

private Value throw_(ref Value msg) /*pure*/ {
    throw new ThrownException(coerceToString(msg));
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
            auto s = tryAttrsToString(v, false);
            if (s !is null) return escapeString(s);
            s = "{";
            bool first = true;
            // TODO: sort by key
            foreach (k, e; v.attrs) {
                if (!first) s ~= ',';
                first = false;
                s ~= escapeString(k)~":"~convJSON(*e);
            }
            return s ~ '}';
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
    throw new AbortException(coerceToString(msg));
}

private Value elemAt(ref Value list, NixInt n) /*pure*/ {
    return forceValue(*forceValue(list).list[n]);
}

private Value elemAt(ref Value list, ref Value index) /*pure*/ {
    return elemAt(list, forceValue(index).integer);
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
    return *convJSON(parseJSON(forceValue(str).str));
}

private Value intersectAttrs(ref Value e1, ref Value e2) {
    forceValue(e1);
    forceValue(e2);
    Bindings set;
    foreach (k, v; e1.attrs) {
        auto j = k in e2.attrs;
        if (j) set[k] = *j;
    }
    return Value(set);
}

private Value import_(ref Value filename) /*pure*/ {
    import std.file : readText;
    auto s = readText(coerceToPath(filename));
    auto tr = TokenRange!string(s);
    return eval(parse(tr));
}

private Value getAttr(ref Value name, ref Value attrs) {
    return *forceValue(attrs).attrs[forceValue(name).str];
}

private Value hasAttr(ref Value name, ref Value attrs) {
    return Value((forceValue(name).str in forceValue(attrs).attrs) !is null);
}

private Value attrValues(ref Value attrs) {
    import std.algorithm : sort;
    Value*[] values;
    auto aa = forceValue(attrs).attrs;
    foreach (name; aa.keys.sort) {
        values ~= aa[name];
    }
    return Value(values);
}

private Value attrNames(ref Value attrs) {
    import std.algorithm : sort;
    Value*[] names;
    auto aa = forceValue(attrs).attrs;
    foreach (name; aa.keys.sort) {
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
    import std.path : dirName;
    const dir = dirName(coerceToString(file));
    return file.type == Type.Path ? Value(dir) : Value(dir, file.context);
}

private Value catAttrs(ref Value v, ref Value listOfAttrs) {
    Value*[] list;
    const attrName = forceValue(v).str;
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
    import std.path : baseName;
    const dir = baseName(coerceToString(file));
    return file.type == Type.Path ? Value(dir) : Value(dir, file.context);
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
    return Value(s[start..end], str.context);
}

private Value removeAttrs(ref Value attrs, ref Value list) {
    auto set = forceValue(attrs).attrs.dup;
    foreach (v; forceValue(list).list) {
        set.remove(forceValue(*v).str);
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

private Value derivation(ref Value attrs) {
    auto drvAttrs = forceValue(attrs).attrs.dup;
    const name = forceValue(*drvAttrs["name"]).str;
    auto drv = new Value(drvAttrs);
    drvAttrs["all"] = new Value([drv]);
    drvAttrs["drvAttrs"] = new Value(drvAttrs);
    drvAttrs["drvPath"] = new Value("/nix/store/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-"~name~".drv", null);
    drvAttrs["out"] = drv;
    drvAttrs["outPath"] = new Value("/nix/store/yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy-"~name, null);
    drvAttrs["outputName"] = new Value("out", null);
    drvAttrs["type"] = new Value("derivation", null);
    return *drv;
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

Env staticBaseEnv;

static this() {
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

    Bindings globals = [
        "abort" : wrap!abort,
        "__add" : wrap!(binOp!"+"),
        "__addErrorContext" : ni!"__addErrorContext",
        "__all" : wrap!all,
        "__any" : wrap!any,
        "__appendContext" : ni!"__appendContext",
        "__attrNames" : wrap!attrNames,
        "__attrValues" : wrap!attrValues,
        "baseNameOf" : wrap!baseNameOf,
        "__bitAnd" :  wrap!(binOp!"&"),
        "__bitOr" :  wrap!(binOp!"|"),
        "__bitXor" :  wrap!(binOp!"^"),
        "__catAttrs" : wrap!catAttrs,
        "__compareVersions" : ni!"__compareVersions",
        "__concatLists" : wrap!concatLists,
        "__concatMap" : wrap!concatMap,
        "__concatStringsSep" : wrap!concatStringsSep,
        "__currentSystem" : new Value("x86_64-darwin", null),
        "__currentTime" : new Value(time(null)),
        "__deepSeq" : wrap!deepSeq,
        "derivation" : wrap!derivation, //lambda
        "derivationStrict" : ni!"derivationStrict",
        "dirOf" : wrap!dirOf,
        "__div" : wrap!(binOp!"/"),
        "__elem" : wrap!elem,
        "__elemAt" : wrap!elemAt,
        "false" : new Value(false),
        "fetchGit" : ni!"fetchGit",
        "fetchMercurial" : ni!"fetchMercurial",
        "fetchTarball" : ni!"fetchTarball",
        "__fetchurl" : ni!"__fetchurl",
        "__filter" : wrap!filter,
        "__filterSource" : ni!"__filterSource",
        "__findFile" : ni!"__findFile",
        "__foldl'" : wrap!foldl_,
        "__fromJSON" : wrap!fromJSON,
        "fromTOML" : ni!"fromTOML",
        "__functionArgs" : wrap!functionArgs,
        "__genList" : wrap!genList,
        "__genericClosure" : wrap!genericClosure,
        "__getAttr" : wrap!getAttr,
        "__getContext" : ni!"__getContext",
        "__getEnv" : ni!"__getEnv",
        "__hasAttr" : wrap!hasAttr,
        "__hasContext" : ni!"__hasContext",
        "__hashFile" : ni!"__hashFile",
        "__hashString" : ni!"__hashString",
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
        "__langVersion" : new Value(5),
        "__length" : wrap!length_,
        "__lessThan" : wrap!lessThan,
        "__listToAttrs" : ni!"__listToAttrs",
        "map" : wrap!map,
        "__mapAttrs" : ni!"__mapAttrs",
        "__match" : ni!"__match",
        "__mul" : wrap!(binOp!"*"),
        "__nixPath" : new Value(cast(Value*[])[]),
        "__nixVersion" : new Value("2.3.4", null), //FIXME
        "null" : new Value,
        "__parseDrvName" : ni!"__parseDrvName",
        "__partition" : ni!"__partition",
        "__path" : ni!"__path",
        "__pathExists" : ni!"__pathExists",
        "placeholder" : ni!"placeholder",
        "__readDir" : ni!"__readDir",
        "__readFile" : ni!"__readFile",
        "removeAttrs" : wrap!removeAttrs,
        "__replaceStrings" : ni!"__replaceStrings",
        "scopedImport" : ni!"scopedImport",
        "__seq" : wrap!seq,
        "__sort" : wrap!sort,
        "__split" : ni!"__split",
        "__splitVersion" : ni!"__splitVersion",
        "__storeDir" : new Value("/nix/store"),
        "__storePath" : ni!"__storePath",
        "__stringLength" : wrap!stringLength,
        "__sub" : wrap!(binOp!"-"),
        "__substring" : wrap!substring,
        "__tail" : wrap!tail,
        "throw" : wrap!(throw_),
        "__toFile" : ni!"__toFile",
        "__toJSON" : wrap!toJSON,
        "__toPath" : ni!"__toPath",
        "toString" : wrap!(toString),
        "__toXML" : ni!"__toXML",
        "__trace" : ni!"__trace",
        "true" : new Value(true),
        "__tryEval" : wrap!tryEval,
        "__typeOf" : wrap!typeOf_,
        "__unsafeDiscardOutputDependency" : ni!"__unsafeDiscardOutputDependency",
        "__unsafeDiscardStringContext" : wrap!unsafeDiscardStringContext,
        "__unsafeGetAttrPos" : ni!"__unsafeGetAttrPos",
    ];

    Bindings builtins;
    foreach (k, v; globals) {
        import std.string : strip;
        builtins[strip(k, "_")] = v;
    }
    globals["builtins"] = builtins["builtins"] = new Value(builtins);
    staticBaseEnv.vars = globals;
}

unittest {
    assert(staticBaseEnv.up is null);
    assert(staticBaseEnv.vars["builtins"].type == Type.Attrs);
    assert(staticBaseEnv.vars["builtins"].attrs["builtins"].type == Type.Attrs);
}
