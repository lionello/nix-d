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

private Value typeOf_(ref Value arg) /*pure*/ {
    return Value(forceValue(arg).typeOf, null);
}

private Value isNull(ref Value arg) /*pure*/ {
    return Value(forceValue(arg).isNull);
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

private Value import_(ref Value filename) /*pure*/ {
    import std.file : readText;
    auto s = readText(coerceToPath(filename));
    auto tr = TokenRange!string(s);
    // scope (failure)
    //     writeln(filename, ", error on line ", tr.front.loc.line);
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
        debug writeln("key=",key);
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

private Value substring(ref Value from, ref Value len, ref Value str) {
    const s = coerceToString(str);
    auto start = forceValue(from).integer;
    auto end = start + forceValue(len).integer;
    if (start >= s.length) start = end = 0;
    else if (end > s.length) end = s.length;
    return Value(s[start..end], str.context);
}

private Value seq(ref Value a, ref Value b) {
    forceValue(a);
    return forceValue(b);
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

private Value* wrap(alias F)() {
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

Env staticBaseEnv;

static this() {
    import core.stdc.time : time;

    static Value notImplemented(Value*[] args...) /*pure*/ {
        assert(0, "not implemented");
    }
    auto ni = new Value(&notImplemented);

    Bindings globals = [
        "abort" : wrap!abort(),
        "__add" : wrap!(binOp!"+")(),
        "__addErrorContext" : ni,
        "__all" : wrap!all(),
        "__any" : wrap!any(),
        "__appendContext" : ni,
        "__attrNames" : wrap!attrNames(),
        "__attrValues" : wrap!attrValues(),
        "baseNameOf" : wrap!baseNameOf(),
        "__bitAnd" :  wrap!(binOp!"&")(),
        "__bitOr" :  wrap!(binOp!"|")(),
        "__bitXor" :  wrap!(binOp!"^")(),
        "__catAttrs" : wrap!catAttrs(),
        "__compareVersions" : ni,
        "__concatLists" : ni,
        "__concatMap" : wrap!concatMap(),
        "__concatStringsSep" : wrap!concatStringsSep(),
        "__currentSystem" : new Value("x86_64-darwin", null),
        "__currentTime" : new Value(time(null)),
        "__deepSeq" : wrap!deepSeq(),
        "derivation" : wrap!derivation(), //lambda
        "derivationStrict" : ni,
        "dirOf" : wrap!dirOf(),
        "__div" : wrap!(binOp!"/")(),
        "__elem" : wrap!elem(),
        "__elemAt" : wrap!elemAt(),
        "false" : new Value(false),
        "fetchGit" : ni,
        "fetchMercurial" : ni,
        "fetchTarball" : ni,
        "__fetchurl" : ni,
        "__filter" : ni,
        "__filterSource" : ni,
        "__findFile" : ni,
        "__foldl'" : wrap!foldl_(),
        "__fromJSON" : ni,
        "fromTOML" : ni,
        "__functionArgs" : ni,
        "__genList" : wrap!genList(),
        "__genericClosure" : wrap!genericClosure(),
        "__getAttr" : wrap!getAttr(),
        "__getContext" : ni,
        "__getEnv" : ni,
        "__hasAttr" : wrap!hasAttr(),
        "__hasContext" : ni,
        "__hashFile" : ni,
        "__hashString" : ni,
        "__head" : wrap!head(),
        "import" : wrap!import_(),
        "__intersectAttrs" : ni,
        "__isAttrs" : ni,
        "__isBool" : ni,
        "__isFloat" : ni,
        "__isFunction" : ni,
        "__isInt" : ni,
        "__isList" : ni,
        "isNull" : wrap!isNull(),
        "__isPath" : ni,
        "__isString" : ni,
        "__langVersion" : new Value(5),
        "__length" : wrap!length_(),
        "__lessThan" : wrap!lessThan(),
        "__listToAttrs" : ni,
        "map" : wrap!map(),
        "__mapAttrs" : ni,
        "__match" : ni,
        "__mul" : wrap!(binOp!"*")(),
        "__nixPath" : new Value(cast(Value*[])[]),
        "__nixVersion" : new Value("2.3.4", null), //FIXME
        "null" : new Value(),
        "__parseDrvName" : ni,
        "__partition" : ni,
        "__path" : ni,
        "__pathExists" : ni,
        "placeholder" : ni,
        "__readDir" : ni,
        "__readFile" : ni,
        "removeAttrs" : ni,
        "__replaceStrings" : ni,
        "scopedImport" : ni,
        "__seq" : wrap!seq(),
        "__sort" : ni,
        "__split" : ni,
        "__splitVersion" : ni,
        "__storeDir" : new Value("/nix/store"),
        "__storePath" : ni,
        "__stringLength" : ni,
        "__sub" : wrap!(binOp!"-")(),
        "__substring" : wrap!substring(),
        "__tail" : wrap!tail(),
        "throw" : wrap!(throw_)(),
        "__toFile" : ni,
        "__toJSON" : ni,
        "__toPath" : ni,
        "toString" : wrap!(toString)(),
        "__toXML" : ni,
        "__trace" : ni,
        "true" : new Value(true),
        "__tryEval" : ni,
        "__typeOf" : wrap!typeOf_(),
        "__unsafeDiscardOutputDependency" : ni,
        "__unsafeDiscardStringContext" : wrap!unsafeDiscardStringContext(),
        "__unsafeGetAttrPos" : ni,
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
