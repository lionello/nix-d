module nix.primops;

import nix.value, nix.evaluator;

private Value genList(Value func, Value count) {
    const length = forceValue(count).integer;
    Value[] list;
    foreach (n; 0..length) {
        list ~= callFunction(func, Value(n), Loc());
    }
    return Value(list);
}

private Value prim_any(in Value[] args...) /*pure*/ {
    if (args.length < 2) return Value(&prim_any, args);
    assert(args.length == 2);
    foreach (e; forceValue(args[1], Loc()).list) {
        if (callFunction(args[0], e, Loc()).boolean)
            return Value.TRUE;
    }
    return Value.FALSE;
}

private Value prim_all(in Value[] args...) /*pure*/ {
    if (args.length < 2) return Value(&prim_all, args);
    assert(args.length == 2);
    foreach (e; forceValue(args[1], Loc()).list) {
        if (!callFunction(args[0], e, Loc()).boolean)
            return Value.FALSE;
    }
    return Value.TRUE;
}

private Value prim_binOp(string OP)(in Value[] args...) /*pure*/ {
    assert(args.length == 2);
    return args[0].opBinary!OP(args[1]);
}

private Value binOp(string OP)(Value lhs, Value rhs) /*pure*/ {
    return forceValue(rhs).opBinary!OP(forceValue(lhs));
}

private Value prim_typeOf(in Value[] args...) /*pure*/ {
    if (args.length < 1) return Value(&mixin(__FUNCTION__), args);
    assert(args.length == 1);
    return Value(forceValue(args[0], Loc()).typeOf, null);
}

private Value prim_isNull(in Value[] args...) /*pure*/ {
    if (args.length < 1) return Value(&mixin(__FUNCTION__), args);
    assert(args.length == 1);
    return Value(args[0].isNull);
}

private Value prim_lessThan(in Value[] args...) /*pure*/ {
    if (args.length < 2) return Value(&mixin(__FUNCTION__), args);
    assert(args.length == 2);
    return Value(args[0] < args[1]);
}
private Value prim_toString(in Value[] args...) /*pure*/ {
    if (args.length < 1) return Value(&mixin(__FUNCTION__), args);
    assert(args.length == 1);
    return Value(coerceToString(args[0], true));
}

private Value prim_throw(in Value[] args...) /*pure*/ {
    if (args.length < 1) return Value(&mixin(__FUNCTION__), args);
    assert(args.length == 1);
    throw new Error(args[0].str);
}

private Value prim_abort(in Value[] args...) /*pure*/ {
    if (args.length < 1) return Value(&mixin(__FUNCTION__), args);
    assert(args.length == 1);
    throw new Error("evaluation aborted with the following error message: "~args[0].str);
}

private Value elemAt(in Value list, long n) pure {
    return list.list[n];
}

private Value primop_elemAt(in Value[] args...) /*pure*/ {
    if (args.length < 2) return Value(&mixin(__FUNCTION__), args);
    assert(args.length == 2);
    return elemAt(args[0], args[1].integer);
}

private Value primop_head(in Value[] args...) /*pure*/ {
    if (args.length < 1) return Value(&mixin(__FUNCTION__), args);
    assert(args.length == 1);
    return elemAt(args[0], 0);
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

const Env staticBaseEnv;

static this() {
    import core.stdc.time : time;

    static Value notImplemented(in Value[] args...) /*pure*/ {
        assert(0, "not implemented");
    }

    Bindings globals = [
        "abort" : Value(&prim_abort), "__add" : wrap!(binOp!"+")(),
        "__addErrorContext" : Value(&notImplemented), "__all" : Value(&prim_all),
        "__any" : Value(&prim_any), "__appendContext" : Value(&notImplemented),
        "__attrNames" : Value(&notImplemented), "__attrValues" : Value(&notImplemented),
        "baseNameOf" : Value(&notImplemented), "__bitAnd" :  wrap!(binOp!"&")(),
        "__bitOr" :  wrap!(binOp!"|")(), "__bitXor" :  wrap!(binOp!"^")(),
        "__catAttrs" : Value(&notImplemented), "__compareVersions" : Value(&notImplemented),
        "__concatLists" : Value(&notImplemented), "__concatMap" : Value(&notImplemented),
        "__concatStringsSep" : Value(&notImplemented), "__currentSystem" : Value("x86_64-darwin", null),
        "__currentTime" : Value(time(null)), "__deepSeq" : Value(&notImplemented),
        "derivation" : Value(&notImplemented), //lambda
        "derivationStrict" : Value(&notImplemented),
        "dirOf" : Value(&notImplemented), "__div" : Value(&prim_binOp!"/"),
        "__elem" : Value(&notImplemented), "__elemAt" : Value(&primop_elemAt),
        "false" : Value(false), "fetchGit" : Value(&notImplemented),
        "fetchMercurial" : Value(&notImplemented), "fetchTarball" : Value(&notImplemented),
        "__fetchurl" : Value(&notImplemented), "__filter" : Value(&notImplemented),
        "__filterSource" : Value(&notImplemented), "__findFile" : Value(&notImplemented),
        "__foldl'" : wrap!foldl_(), "__fromJSON" : Value(&notImplemented),
        "fromTOML" : Value(&notImplemented), "__functionArgs" : Value(&notImplemented),
        "__genList" : wrap!genList(), "__genericClosure" : Value(&notImplemented),
        "__getAttr" : Value(&notImplemented), "__getContext" : Value(&notImplemented),
        "__getEnv" : Value(&notImplemented), "__hasAttr" : Value(&notImplemented),
        "__hasContext" : Value(&notImplemented), "__hashFile" : Value(&notImplemented),
        "__hashString" : Value(&notImplemented), "__head" : Value(&primop_head),
        "import" : wrap!import_(),
        "__intersectAttrs" : Value(&notImplemented),
        "__isAttrs" : Value(&notImplemented),
        "__isBool" : Value(&notImplemented), "__isFloat" : Value(&notImplemented),
        "__isFunction" : Value(&notImplemented), "__isInt" : Value(&notImplemented),
        "__isList" : Value(&notImplemented), "isNull" : Value(&prim_isNull),
        "__isPath" : Value(&notImplemented), "__isString" : Value(&notImplemented),
        "__langVersion" : Value(5), "__length" : Value(&notImplemented),
        "__lessThan" : Value(&prim_lessThan), "__listToAttrs" : Value(&notImplemented),
        "map" : Value(&notImplemented), "__mapAttrs" : Value(&notImplemented),
        "__match" : Value(&notImplemented), "__mul" : Value(&prim_binOp!"*"),
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
        "__sub" : Value(&prim_binOp!"-"), "__substring" : Value(&notImplemented),
        "__tail" : Value(&notImplemented), "throw" : Value(&prim_throw),
        "__toFile" : Value(&notImplemented), "__toJSON" : Value(&notImplemented),
        "__toPath" : Value(&notImplemented), "toString" : Value(&prim_toString),
        "__toXML" : Value(&notImplemented), "__trace" : Value(&notImplemented),
        "true" : Value(true), "__tryEval" : Value(&notImplemented),
        "__typeOf" : Value(&prim_typeOf),
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
