module nix.primops;

debug import std.stdio:writeln;

import nix.value, nix.evaluator;

private Value genList(Value func, Value count) {
    const length = forceValue(count).integer;
    Value[] list;
    foreach (n; 0..length) {
        list ~= callFunction(func, Value(n), Loc());
    }
    return Value(list);
}

private Value any(in Value fun, in Value list) /*pure*/ {
    foreach (e; forceValue(list).list) {
        if (callFunction(fun, e, Loc()).boolean)
            return Value.TRUE;
    }
    return Value.FALSE;
}

private Value all(in Value fun, in Value list) /*pure*/ {
    foreach (e; forceValue(list).list) {
        if (!callFunction(fun, e, Loc()).boolean)
            return Value.FALSE;
    }
    return Value.TRUE;
}

private Value binOp(string OP)(Value lhs, Value rhs) /*pure*/ {
    return forceValue(lhs).opBinary!OP(forceValue(rhs));
}

private Value typeOf_(in Value arg) /*pure*/ {
    return Value(forceValue(arg).typeOf, null);
}

private Value isNull(in Value arg) /*pure*/ {
    return Value(forceValue(arg).isNull);
}

private Value lessThan(in Value lhs, in Value rhs) /*pure*/ {
    return Value(forceValue(lhs) < forceValue(rhs));
}

private Value toString(in Value arg) /*pure*/ {
    return Value(coerceToString(arg, true), null);
}

private Value throw_(in Value msg) /*pure*/ {
    throw new Error(msg.str);
}

private Value abort(in Value msg) /*pure*/ {
    throw new Error("evaluation aborted with the following error message: "~msg.str);
}

private Value elemAt(in Value list, in Value index) /*pure*/ {
    return forceValue(list).list[forceValue(index).integer];
}

private Value head(in Value list) /*pure*/ {
    return forceValue(list).list[0];
}

private Value foldl_(Value func, Value acc, Value list) {
    foreach (e; forceValue(list).list) {
        acc = callFunction(callFunction(func, acc, Loc()), e, Loc());
    }
    return acc;
}

private Value import_(in Value filename) /*pure*/ {
    import std.file : readText;
    auto s = readText(coerceToPath(filename));
    auto tr = TokenRange!string(s);
    // scope (failure)
    //     writeln(filename, ", error on line ", tr.front.loc.line);
    return eval(parse(tr));
}

private Value wrap(alias F)() {
    import std.traits : arity;
    static Value primop(in Value[] args...) {
        if (args.length < arity!F) return Value(&primop, args);
        assert(args.length == arity!F);
        static if (arity!F == 0) return F();
        static if (arity!F == 1) return F(args[0]);
        static if (arity!F == 2) return F(args[0], args[1]);
        static if (arity!F == 3) return F(args[0], args[1], args[2]);
    }
    return Value(&primop);
}

private Value getAttr(Value name, Value attrs) {
    return forceValue(attrs).attrs[forceValue(name).str];
}

private Value hasAttr(Value name, Value attrs) {
    return Value((forceValue(name).str in forceValue(attrs).attrs) !is null);
}

private Value attrValues(Value attrs) {
    import std.algorithm : sort;
    Value[] values;
    auto aa = forceValue(attrs).attrs;
    foreach (name; aa.keys.sort) {
        values ~= aa[name];
    }
    debug writeln("attrValues=",values);
    return Value(values);
}

private Value attrNames(Value attrs) {
    import std.algorithm : sort;
    Value[] names;
    foreach (name; forceValue(attrs).attrs.keys.sort) {
        names ~= Value(name, null);
    }
    debug writeln("attrNames=",names);
    return Value(names);
}

private Value map(Value fun, Value list) {
    Value[] output;
    foreach (e; forceValue(list).list) {
        output ~= callFunction(fun, e, Loc());
    }
    debug writeln("map=",output);
    return Value(output);
}

private Value tail(Value list) {
    return Value(forceValue(forceValue(list)).list[1..$]);
}

private Value dirOf(Value file) {
    import std.path : dirName;
    const dir = dirName(coerceToString(file));
    return file.type == Type.Path ? Value(dir) : Value(dir, null);
}

private Value catAttrs(Value v, Value listOfAttrs) {
    Value[] list;
    const attrName = forceValue(v).str;
    foreach (e; forceValue(listOfAttrs).list) {
        const attr = attrName in forceValue(e).attrs;
        if (attr) list ~= *attr;
    }
    return Value(list);
}

private Value length_(Value list) {
    return Value(forceValue(forceValue(list)).list.length);
}

private Value genericClosure(Value attr) {
    const attrs = forceValue(attr).attrs;
    auto startSet = forceValue(attrs["startSet"]).list;
    const operator = forceValue(attrs["operator"]);
    bool[Value] done;
    Value[] res;
    while (startSet.length) {
        const e = forceValue(startSet[0]);
        startSet = startSet[1..$];
        const key = forceValue(e.attrs["key"]);
        debug writeln("key=",key);
        if (key in done) continue;
        done[key] = true;
        assert(key in done);
        res ~= e;
        auto result = callFunction(operator, e, Loc());
        startSet ~= forceValue(result).list;
    }
    return Value(res);
}

const Env staticBaseEnv;

static this() {
    import core.stdc.time : time;

    static Value notImplemented(in Value[] args...) /*pure*/ {
        assert(0, "not implemented");
    }

    Bindings globals = [
        "abort" : wrap!abort(), "__add" : wrap!(binOp!"+")(),
        "__addErrorContext" : Value(&notImplemented), "__all" : wrap!all(),
        "__any" : wrap!any(), "__appendContext" : Value(&notImplemented),
        "__attrNames" : wrap!attrNames(), "__attrValues" : wrap!attrValues(),
        "baseNameOf" : Value(&notImplemented), "__bitAnd" :  wrap!(binOp!"&")(),
        "__bitOr" :  wrap!(binOp!"|")(), "__bitXor" :  wrap!(binOp!"^")(),
        "__catAttrs" : wrap!catAttrs(), "__compareVersions" : Value(&notImplemented),
        "__concatLists" : Value(&notImplemented), "__concatMap" : Value(&notImplemented),
        "__concatStringsSep" : Value(&notImplemented), "__currentSystem" : Value("x86_64-darwin", null),
        "__currentTime" : Value(time(null)), "__deepSeq" : Value(&notImplemented),
        "derivation" : Value(&notImplemented), //lambda
        "derivationStrict" : Value(&notImplemented),
        "dirOf" : wrap!dirOf(), "__div" : wrap!(binOp!"/")(),
        "__elem" : Value(&notImplemented), "__elemAt" : wrap!elemAt(),
        "false" : Value(false), "fetchGit" : Value(&notImplemented),
        "fetchMercurial" : Value(&notImplemented), "fetchTarball" : Value(&notImplemented),
        "__fetchurl" : Value(&notImplemented), "__filter" : Value(&notImplemented),
        "__filterSource" : Value(&notImplemented), "__findFile" : Value(&notImplemented),
        "__foldl'" : wrap!foldl_(), "__fromJSON" : Value(&notImplemented),
        "fromTOML" : Value(&notImplemented), "__functionArgs" : Value(&notImplemented),
        "__genList" : wrap!genList(), "__genericClosure" : wrap!genericClosure(),
        "__getAttr" : wrap!getAttr(), "__getContext" : Value(&notImplemented),
        "__getEnv" : Value(&notImplemented), "__hasAttr" : wrap!hasAttr(),
        "__hasContext" : Value(&notImplemented), "__hashFile" : Value(&notImplemented),
        "__hashString" : Value(&notImplemented), "__head" : wrap!head(),
        "import" : wrap!import_(),
        "__intersectAttrs" : Value(&notImplemented),
        "__isAttrs" : Value(&notImplemented),
        "__isBool" : Value(&notImplemented), "__isFloat" : Value(&notImplemented),
        "__isFunction" : Value(&notImplemented), "__isInt" : Value(&notImplemented),
        "__isList" : Value(&notImplemented), "isNull" : wrap!isNull(),
        "__isPath" : Value(&notImplemented), "__isString" : Value(&notImplemented),
        "__langVersion" : Value(5), "__length" : wrap!length_(),
        "__lessThan" : wrap!lessThan(), "__listToAttrs" : Value(&notImplemented),
        "map" : wrap!map(), "__mapAttrs" : Value(&notImplemented),
        "__match" : Value(&notImplemented), "__mul" : wrap!(binOp!"*")(),
        "__nixPath" : Value(cast(Value[])[]), "__nixVersion" : Value("2.3.4", null), //FIXME
        "null" : Value(), "__parseDrvName" : Value(&notImplemented),
        "__partition" : Value(&notImplemented), "__path" : Value(&notImplemented),
        "__pathExists" : Value(&notImplemented), "placeholder" : Value(&notImplemented),
        "__readDir" : Value(&notImplemented), "__readFile" : Value(&notImplemented),
        "removeAttrs" : Value(&notImplemented), "__replaceStrings" : Value(&notImplemented),
        "scopedImport" : Value(&notImplemented), "__seq" : Value(&notImplemented),
        "__sort" : Value(&notImplemented), "__split" : Value(&notImplemented),
        "__splitVersion" : Value(&notImplemented), "__storeDir" : Value("/nix/store"),
        "__storePath" : Value(&notImplemented), "__stringLength" : Value(&notImplemented),
        "__sub" : wrap!(binOp!"-")(), "__substring" : Value(&notImplemented),
        "__tail" : wrap!tail(), "throw" : wrap!(throw_)(),
        "__toFile" : Value(&notImplemented), "__toJSON" : Value(&notImplemented),
        "__toPath" : Value(&notImplemented), "toString" : wrap!(toString)(),
        "__toXML" : Value(&notImplemented), "__trace" : Value(&notImplemented),
        "true" : Value(true), "__tryEval" : Value(&notImplemented),
        "__typeOf" : wrap!typeOf_(),
        "__unsafeDiscardOutputDependency" : Value(&notImplemented),
        "__unsafeDiscardStringContext" : Value(&notImplemented),
        "__unsafeGetAttrPos" : Value(&notImplemented), "__valueSize" : Value(&notImplemented),
    ];

    Bindings builtins;
    foreach (k, v; globals) {
        import std.string : strip;
        builtins[strip(k, "_")] = v;
    }
    globals["builtins"] = builtins["builtins"] = Value(builtins);
    staticBaseEnv.vars = globals;
}

unittest {
    assert(staticBaseEnv.up is null);
    assert(staticBaseEnv.vars["builtins"].type == Type.Attrs);
    assert(staticBaseEnv.vars["builtins"].attrs["builtins"].type == Type.Attrs);
}
