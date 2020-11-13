module nix.primops;

debug import std.stdio:writeln;

import nix.value, nix.evaluator;

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
    return Value(coerceToString(arg, true), null);
}

private Value throw_(ref Value msg) /*pure*/ {
    throw new Error(msg.str);
}

private Value abort(ref Value msg) /*pure*/ {
    throw new Error("evaluation aborted with the following error message: "~msg.str);
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
    return file.type == Type.Path ? Value(dir) : Value(dir, null);
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
    return file.type == Type.Path ? Value(dir) : Value(dir, null);
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
    foreach (i, e; forceValue(list).list) {
        if (i) s ~= sep.str;
        s ~= e.str;
    }
    return Value(s, null);
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

    Bindings globals = [
        "abort" : wrap!abort(),
        "__add" : wrap!(binOp!"+")(),
        "__addErrorContext" : new Value(&notImplemented),
        "__all" : wrap!all(),
        "__any" : wrap!any(),
        "__appendContext" : new Value(&notImplemented),
        "__attrNames" : wrap!attrNames(),
        "__attrValues" : wrap!attrValues(),
        "baseNameOf" : wrap!baseNameOf(),
        "__bitAnd" :  wrap!(binOp!"&")(),
        "__bitOr" :  wrap!(binOp!"|")(),
        "__bitXor" :  wrap!(binOp!"^")(),
        "__catAttrs" : wrap!catAttrs(),
        "__compareVersions" : new Value(&notImplemented),
        "__concatLists" : new Value(&notImplemented),
        "__concatMap" : wrap!concatMap(),
        "__concatStringsSep" : wrap!concatStringsSep(),
        "__currentSystem" : new Value("x86_64-darwin", null),
        "__currentTime" : new Value(time(null)),
        "__deepSeq" : new Value(&notImplemented),
        "derivation" : new Value(&notImplemented), //lambda
        "derivationStrict" : new Value(&notImplemented),
        "dirOf" : wrap!dirOf(),
        "__div" : wrap!(binOp!"/")(),
        "__elem" : new Value(&notImplemented),
        "__elemAt" : wrap!elemAt(),
        "false" : new Value(false),
        "fetchGit" : new Value(&notImplemented),
        "fetchMercurial" : new Value(&notImplemented),
        "fetchTarball" : new Value(&notImplemented),
        "__fetchurl" : new Value(&notImplemented),
        "__filter" : new Value(&notImplemented),
        "__filterSource" : new Value(&notImplemented),
        "__findFile" : new Value(&notImplemented),
        "__foldl'" : wrap!foldl_(),
        "__fromJSON" : new Value(&notImplemented),
        "fromTOML" : new Value(&notImplemented),
        "__functionArgs" : new Value(&notImplemented),
        "__genList" : wrap!genList(),
        "__genericClosure" : wrap!genericClosure(),
        "__getAttr" : wrap!getAttr(),
        "__getContext" : new Value(&notImplemented),
        "__getEnv" : new Value(&notImplemented),
        "__hasAttr" : wrap!hasAttr(),
        "__hasContext" : new Value(&notImplemented),
        "__hashFile" : new Value(&notImplemented),
        "__hashString" : new Value(&notImplemented),
        "__head" : wrap!head(),
        "import" : wrap!import_(),
        "__intersectAttrs" : new Value(&notImplemented),
        "__isAttrs" : new Value(&notImplemented),
        "__isBool" : new Value(&notImplemented),
        "__isFloat" : new Value(&notImplemented),
        "__isFunction" : new Value(&notImplemented),
        "__isInt" : new Value(&notImplemented),
        "__isList" : new Value(&notImplemented),
        "isNull" : wrap!isNull(),
        "__isPath" : new Value(&notImplemented),
        "__isString" : new Value(&notImplemented),
        "__langVersion" : new Value(5),
        "__length" : wrap!length_(),
        "__lessThan" : wrap!lessThan(),
        "__listToAttrs" : new Value(&notImplemented),
        "map" : wrap!map(),
        "__mapAttrs" : new Value(&notImplemented),
        "__match" : new Value(&notImplemented),
        "__mul" : wrap!(binOp!"*")(),
        "__nixPath" : new Value(cast(Value*[])[]),
        "__nixVersion" : new Value("2.3.4", null), //FIXME
        "null" : new Value(),
        "__parseDrvName" : new Value(&notImplemented),
        "__partition" : new Value(&notImplemented),
        "__path" : new Value(&notImplemented),
        "__pathExists" : new Value(&notImplemented),
        "placeholder" : new Value(&notImplemented),
        "__readDir" : new Value(&notImplemented),
        "__readFile" : new Value(&notImplemented),
        "removeAttrs" : new Value(&notImplemented),
        "__replaceStrings" : new Value(&notImplemented),
        "scopedImport" : new Value(&notImplemented),
        "__seq" : new Value(&notImplemented),
        "__sort" : new Value(&notImplemented),
        "__split" : new Value(&notImplemented),
        "__splitVersion" : new Value(&notImplemented),
        "__storeDir" : new Value("/nix/store"),
        "__storePath" : new Value(&notImplemented),
        "__stringLength" : new Value(&notImplemented),
        "__sub" : wrap!(binOp!"-")(),
        "__substring" : new Value(&notImplemented),
        "__tail" : wrap!tail(),
        "throw" : wrap!(throw_)(),
        "__toFile" : new Value(&notImplemented),
        "__toJSON" : new Value(&notImplemented),
        "__toPath" : new Value(&notImplemented),
        "toString" : wrap!(toString)(),
        "__toXML" : new Value(&notImplemented),
        "__trace" : new Value(&notImplemented),
        "true" : new Value(true),
        "__tryEval" : new Value(&notImplemented),
        "__typeOf" : wrap!typeOf_(),
        "__unsafeDiscardOutputDependency" : new Value(&notImplemented),
        "__unsafeDiscardStringContext" : new Value(&notImplemented),
        "__unsafeGetAttrPos" : new Value(&notImplemented),
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
