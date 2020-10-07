module nix.evaluator;

import nix.value;

import std.stdio : writeln, write;
import std.conv : to;

Value mkThunk(in Expr e, in ref Env env) /*pure*/ {
    return Value(e, env);
}

Value maybeThunk(in Expr e, in ref Env env) /*pure*/ {
    // TODO: don't thunk values or known vars; use Thunker
    return mkThunk(e, env);
}

private Value callPrimOp(in Value fun, in Value arg, in Loc pos) /*pure*/ {
    return Value();
}

private Value callFunction(in Value fun, in Value arg, in Loc pos) /*pure*/ {
    auto f = forceValue(fun, pos);
    switch(f.type) {
    case Type.PrimOp:
    case Type.PrimOpApp:
        return callPrimOp(f, arg, pos);
    case Type.Attrs:
        const functor = f.attrs["__functor"];
        const v2 = callFunction(functor, f, pos);
        return callFunction(v2, arg, pos);
    case Type.Lambda:
        Bindings b;
        auto env2 = Env(f.lambdaEnv, b);
        if (!f.lambda.formals) {
            // add f.lambda.arg to the new env
            b[f.lambda.arg] = arg;
        } else {
            const args = forceAttrs(arg, pos);
            // For each formal argument, get the actual argument.  If
            // there is no matching actual argument but the formal
            // argument has a default, use the default.
            int attrsUsed;
            foreach (formal; f.lambda.formals.elems) {
                const j = formal.name in args.attrs;
                if (!j) {
                    assert(formal.def, "Lambda called without required argument "~formal.name);
                    b[formal.name] = maybeThunk(formal.def, env2);
                } else {
                    attrsUsed++;
                    b[formal.name] = *j;
                }
            }
            if (!f.lambda.formals.ellipsis && attrsUsed != args.attrs.length) {
                foreach (const k, v; args.attrs)
                    assert(0, "Lambda called with unexpected argument "~k);
                assert(0);
            }
        }
        return eval(f.lambda, &env2);
    default:
        assert(0, "attempt to call something which is not a function");
    }
}

private Value forceString(in Value v, in Loc pos) /*pure*/ {
    auto value = forceValue(v, pos);
    assert(value.type == Type.String, "value is "~typeOf(value)~" while a string was expected");
    return value;
}

private Value forceAttrs(in Value v, in Loc pos) /*pure*/ {
    auto value = forceValue(v, pos);
    assert(value.type == Type.Attrs, "value is "~typeOf(value)~" while a set was expected");
    return value;
}

private Value forceValue(in Value v, in Loc pos) /*pure*/ {
    switch (v.type) {
    case Type.Thunk:
        return eval(v.thunk, v.env);
    case Type.App:
        assert(v.list.length == 2);
        return callFunction(v.list[0], v.list[1], pos);
    // case Type.Blackhole:
        // assert(0, "infinite recursion encountered");
    default:
        return v;
    }
}

private Value prim_binOp(string OP)(in Value[] args...) /*pure*/ {
    assert(args.length == 2);
    return args[0].opBinary!OP(args[1]);
}

unittest {
    assert(prim_binOp!"*"(Value(3), Value(2)) == Value(6));
    assert(prim_binOp!"/"(Value(3), Value(2.0)) == Value(1.5));
    assert(prim_binOp!"-"(Value(1.0), Value(2)) == Value(-1.0));
    assert(prim_binOp!"+"(Value(1.0), Value(2.0)) == Value(3.0));
    assert(prim_binOp!"+"(Value("a"), Value("b")) == Value("ab"));
}

private Value prim_typeOf(in Value[] args...) /*pure*/ {
    assert(args.length == 1);
    return Value(forceValue(args[0], Loc()).typeOf);
}

private Value prim_isNull(in Value[] args...) /*pure*/ {
    assert(args.length == 1);
    return Value(args[0].type == Type.Null);
}

private Value prim_lessThan(in Value[] args...) /*pure*/ {
    assert(args.length == 2);
    switch (args[0].type) {
    case Type.Float:
        switch (args[1].type) {
        case Type.Float:
            return Value(args[0].fpoint < args[1].fpoint);
        case Type.Int:
            return Value(args[0].fpoint < args[1].integer);
        default:
            assert(0, "cannot compare");
        }
    case Type.Int:
        switch (args[1].type) {
        case Type.Float:
            return Value(args[0].integer < args[1].fpoint);
        case Type.Int:
            return Value(args[0].integer < args[1].integer);
        default:
            assert(0, "cannot compare");
        }
    case Type.String:
    case Type.Path:
        assert(args[0].type == args[1].type, "cannot compare");
        return Value(args[0].s < args[1].s);
    default:
        assert(0, "cannot compare");
    }
}

private Value lessOrEqThan(in Value[] args...) /*pure*/ {
    assert(args.length == 2);
    switch (args[0].type) {
    case Type.Float:
        switch (args[1].type) {
        case Type.Float:
            return Value(args[0].fpoint <= args[1].fpoint);
        case Type.Int:
            return Value(args[0].fpoint <= args[1].integer);
        default:
            assert(0);
        }
    case Type.Int:
        switch (args[1].type) {
        case Type.Float:
            return Value(args[0].integer <= args[1].fpoint);
        case Type.Int:
            return Value(args[0].integer <= args[1].integer);
        default:
            assert(0);
        }
    case Type.String:
    case Type.Path:
        switch (args[1].type) {
        case Type.String:
        case Type.Path:
            return Value(args[0].s <= args[1].s);
        default:
            assert(0);
        }
    default:
        assert(0);
    }
}

private string coerceToString(in Value v, bool coerceMore = false) /*pure*/ {
    auto value = forceValue(v, Loc());
    switch (value.type) {
    case Type.String:
    case Type.Path:
        return value.s;
    case Type.Attrs:
        auto toString = "__toString" in value.attrs;
        if (toString) {
            auto s = coerceToString(callFunction(*toString, v, Loc()), coerceMore);
            if (s) return s;
        }
        auto i = "outPath" in value.attrs;
        assert(i, "cannot coerce a set to a string");
        return coerceToString(*i, coerceMore);
    // case Type.External: todo
    default:
    }
    if (coerceMore) {
        switch (value.type) {
        case Type.Bool:
            return value.boolean ? "1" : "";
        case Type.Null:
            return "";
        case Type.List:
            string result;
            foreach(i, elem; value.list) {
                if (i) result ~= ' ';
                result ~= coerceToString(elem, coerceMore);
            }
            return result;
        case Type.Int:
        case Type.Float:
            return value.toString();
        default:
        }
    }
    assert(0, "cannot coerce to a string");
}

unittest {
    assert("" == coerceToString(Value()));
}

private Value prim_toString(in Value[] args...) /*pure*/ {
    assert(args.length == 1);
    return Value(args[0].toString());
}

private Value prim_throw(in Value[] args...) /*pure*/ {
    assert(args.length == 1);
    throw new Error(args[0].s);
}

private Value prim_abort(in Value[] args...) /*pure*/ {
    assert(args.length == 1);
    throw new Error("evaluation aborted with the following error message: "~args[0].s);
}

private Value elemAt(in Value list, long n) pure {
    return list.list[n];
}

private Value primop_elemAt(in Value[] args...) /*pure*/ {
    assert(args.length == 2);
    return elemAt(args[0], args[1].integer);
}

private Value primop_head(in Value[] args...) /*pure*/ {
    assert(args.length == 1);
    return elemAt(args[0], 0);
}

private const Env staticBaseEnv;

static this() {
    import core.stdc.time : time;

    static Value notImplemented(in Value[] args...) /*pure*/ {
        assert(0, "not implemented");
    }

    Bindings globals = [
        "abort" : Value(&notImplemented), "__add" : Value(&prim_binOp!"+"),
        "__addErrorContext" : Value(&notImplemented), "__all" : Value(&notImplemented),
        "__any" : Value(&notImplemented), "__appendContext" : Value(&notImplemented),
        "__attrNames" : Value(&notImplemented), "__attrValues" : Value(&notImplemented),
        "baseNameOf" : Value(&notImplemented), "__bitAnd" : Value(&notImplemented),
        "__bitOr" : Value(&notImplemented), "__bitXor" : Value(&notImplemented),
        "__catAttrs" : Value(&notImplemented), "__compareVersions" : Value(&notImplemented),
        "__concatLists" : Value(&notImplemented), "__concatMap" : Value(&notImplemented),
        "__concatStringsSep" : Value(&notImplemented), "__currentSystem" : Value("x86_64-darwin"),
        "__currentTime" : Value(time(null)), //123
        "__deepSeq" : Value(&notImplemented),
        "derivation" : Value(&notImplemented), //lambda
        "derivationStrict" : Value(&notImplemented),
        "dirOf" : Value(&notImplemented), "__div" : Value(&prim_binOp!"/"),
        "__elem" : Value(&notImplemented), "__elemAt" : Value(&primop_elemAt),
        "false" : Value(false), "fetchGit" : Value(&notImplemented),
        "fetchMercurial" : Value(&notImplemented), "fetchTarball" : Value(&notImplemented),
        "__fetchurl" : Value(&notImplemented), "__filter" : Value(&notImplemented),
        "__filterSource" : Value(&notImplemented), "__findFile" : Value(&notImplemented),
        "__foldl'" : Value(&notImplemented), "__fromJSON" : Value(&notImplemented),
        "fromTOML" : Value(&notImplemented), "__functionArgs" : Value(&notImplemented),
        "__genList" : Value(&notImplemented), "__genericClosure" : Value(&notImplemented),
        "__getAttr" : Value(&notImplemented), "__getContext" : Value(&notImplemented),
        "__getEnv" : Value(&notImplemented), "__hasAttr" : Value(&notImplemented),
        "__hasContext" : Value(&notImplemented), "__hashFile" : Value(&notImplemented),
        "__hashString" : Value(&notImplemented), "__head" : Value(&primop_head),
        "import" : Value(&notImplemented), //primOpApp
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
        "__nixPath" : Value.EMPTY, "__nixVersion" : Value("2.3.4"), //FIXME
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
        "__tail" : Value(&notImplemented), "throw" : Value(&notImplemented),
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
    assert(staticBaseEnv.vars["builtins"].type == Type.Attrs);
    assert(staticBaseEnv.vars["builtins"].attrs["builtins"].type == Type.Attrs);
}

/*
class VarBinder : Visitor {
    const(Env)* env = &staticBaseEnv;

    void visit(in Expr e, const ref Env env) {
        if (e) {
            const prevEnv = this.env;
            this.env = &env;
            e.accept(this);
            this.env = prevEnv;
        }
    }

    void visit(in ExprOpNot e) {
        visit(e.expr, *env);
    }

    void visit(in ExprBinaryOp e) {
        visit(e.left, *env);
        visit(e.right, *env);
    }

    void visit(in ExprInt) {
    }

    void visit(in ExprFloat) {
    }

    void visit(in ExprString) {
    }

    void visit(in ExprPath) {
    }

    void visit(in ExprVar e) {
        int withLevel = -1;
        for (auto curEnv = env, level = 0; curEnv; curEnv = curEnv.up, level++) {
            if (curEnv.isWith) {
                if (withLevel == -1)
                    withLevel = level;
            } else {
                if (e.name in curEnv.vars) {
                    // e.fromWith = false;
                    e.level = level;
                    // e.displ =
                    return;
                }
            }
        }
        if (withLevel == -1)
            throw new Error("undefined variable " ~ e.name);
        // e.fromWith = true;
        e.level = withLevel;
    }

    void visit(in ExprSelect e) {
        visit(e.e, *env);
        visit(e.def, *env);
        foreach (a; e.ap)
            visit(a.expr, *env);
    }

    void visit(in ExprOpHasAttr e) {
        visit(e.expr, *env);
        foreach (a; e.ap)
            visit(a.expr, *env);
    }

    void visit(in ExprAttrs e) {
        const(Env)* dynamicEnv = env;
        if (e.recursive) {
            Bindings b;
            foreach (k, v; e.attrs) {
                b[k] = Value();
            }
            auto newEnv = Env(env, false, b);
            dynamicEnv = &newEnv;
            foreach (k, v; e.attrs) {
                visit(v.value, *(v.inherited ? env : &newEnv));
            }
        } else {
            foreach (a; e.attrs) {
                visit(a.value, *env);
            }
        }
        foreach (i; e.dynamicAttrs) {
            visit(i.name, *dynamicEnv);
            visit(i.value, *dynamicEnv);
        }
    }

    void visit(in ExprList e) {
        foreach (i; e.elems)
            visit(i, *env);
    }

    void visit(in ExprLambda e) {
        auto newEnv = Env(this.env);
        if (e.arg)
            newEnv.vars[e.arg] = Value();
        if (e.formals) {
            foreach (f; e.formals.elems) {
                newEnv.vars[f.name] = Value();
            }
            foreach (f; e.formals.elems) {
                if (f.def)
                    visit(f.def, newEnv);
            }
        }
        visit(e.body, newEnv);
    }

    void visit(in ExprLet e) {
        auto newEnv = Env(env);
        foreach (k, v; e.attrs.attrs) {
            newEnv.vars[k] = Value();
        }
        foreach (k, i; e.attrs.attrs) {
            visit(i.value, i.inherited ? *env : newEnv);
        }
        visit(e.body, newEnv);
    }

    void visit(in ExprWith e) {
        visit(e.attrs, *env);
        auto newEnv = Env(env, true);
        visit(e.body, newEnv);
    }

    void visit(in ExprIf e) {
        visit(e.cond, *env);
        visit(e.then, *env);
        visit(e.else_, *env);
    }

    void visit(in ExprAssert e) {
        visit(e.cond, *env);
        visit(e.body, *env);
    }
}
*/


class Thunker : Visitor {
    Env env;
    Value value;
    void thunk(in Expr e) {
        value = Value(e, env);
    }

    void visit(in ExprOpNot e) {
        thunk(e);
    }

    void visit(in ExprBinaryOp e) {
        thunk(e);
    }

    void visit(in ExprInt) {
    }

    void visit(in ExprFloat) {
    }

    void visit(in ExprString) {
    }

    void visit(in ExprPath) {
    }

    void visit(in ExprVar e) {
        // env.vars[e.name]
    }

    void visit(in ExprSelect e) {
        thunk(e);
    }

    void visit(in ExprOpHasAttr e) {
        thunk(e);
    }

    void visit(in ExprAttrs e) {
        thunk(e);
    }

    void visit(in ExprList e) {
        thunk(e);
    }

    void visit(in ExprLambda e) {
        thunk(e);
    }

    void visit(in ExprLet e) {
        thunk(e);
    }

    void visit(in ExprWith e) {
        thunk(e);
    }

    void visit(in ExprIf e) {
        thunk(e);
    }

    void visit(in ExprAssert e) {
        thunk(e);
    }
}

class Evaluator : Visitor {
    const(Env)* env;
    Value value;

    this(const(Env)* env) /*pure*/ {
        this.env = env;
    }

    Value visit(in Expr e) {
        if (e)
            e.accept(this);
        return value;
    }

    void visit(in ExprOpNot e) {
        visit(e.expr);
        assert(value.type == Type.Bool);
        value.boolean = !value.boolean;
    }

    void visit(in ExprBinaryOp e) {
        switch (e.op) {
        case Tok.AND:
            if (visit(e.left).boolean)
                visit(e.right);
            assert(value.type == Type.Bool);
            break;
        case Tok.OR:
            if (!visit(e.left).boolean)
                visit(e.right);
            assert(value.type == Type.Bool);
            break;
        case Tok.EQ:
            value = Value(visit(e.left) == visit(e.right));
            break;
        case Tok.NEQ:
            value = Value(visit(e.left) != visit(e.right));
            break;
        case Tok.CONCAT:
            value = Value(visit(e.left).list ~ visit(e.right).list);
            break;
        case Tok.MUL:
            value = visit(e.left) * visit(e.right);
            break;
        case Tok.DIV:
            value = visit(e.left) / visit(e.right);
            break;
        case Tok.ADD:
            value = visit(e.left) + visit(e.right);
            break;
        case Tok.SUB:
            value = visit(e.left) - visit(e.right);
            break;
        case Tok.UPDATE:
            Bindings b;
            auto orig = visit(e.left);
            assert(orig.type == Type.Attrs);
            foreach (k, v; orig.attrs) {
                b[k] = v;
            }
            auto update = visit(e.right);
            assert(update.type == Type.Attrs);
            foreach (k, v; update.attrs) {
                b[k] = v;
            }
            value = Value(b);
            break;
        case Tok.LT:
            value = prim_lessThan(visit(e.left), visit(e.right));
            break;
        case Tok.LEQ:
            value = lessOrEqThan(visit(e.left), visit(e.right));
            break;
        case Tok.GT:
            value = Value(!lessOrEqThan(visit(e.left), visit(e.right)).boolean);
            break;
        case Tok.GEQ:
            value = Value(!prim_lessThan(visit(e.left), visit(e.right)).boolean);
            break;
        case Tok.IMPL:
            if (visit(e.left).boolean)
                visit(e.right);
            else
                value.boolean = true;
            assert(value.type == Type.Bool);
            break;
        case Tok.APP:
            const args = visit(e.right);
            // TODO: create new Env with all args added
            // Env newEnv;
            visit(visit(e.left).lambda);
            break;
        default:
            assert(0);
        }
    }

    const(Value) lookupVar(string name) {
        for (auto curEnv = this.env; curEnv; curEnv = curEnv.up) {
            // if (curEnv.hasWith) visit(curEnv.hasWith.attrs)
            auto v = name in curEnv.vars;
            if (v)
                return *v;
        }
        assert(0);
    }

    void visit(in ExprInt e) {
        value = Value(e.n);
    }

    void visit(in ExprFloat e) {
        value = Value(e.f);
    }

    void visit(in ExprString e) {
        value = Value(e.s);
    }

    void visit(in ExprPath e) {
        value = Value(e.p);
    }

    void visit(in ExprVar e) {
        value = forceValue(lookupVar(e.name), e.loc);
    }

    string getName(in ref AttrName an) {
        if (an.ident) {
            return an.ident;
        } else {
            visit(an.expr);
            return forceString(value, Loc()).s;
        }
    }

    void visit(in ExprSelect e) {
        visit(e.left);
        foreach (a; e.ap) {
            if (value.type == Type.Attrs) {
                auto j = getName(a) in value.attrs;
                if (j) {
                    value = *j;
                    continue;
                }
            }
            if (e.def) {
                visit(e.def);
                break;
            }
            throw new Error("attribute missing: " ~ a.ident);
        }
    }

    void visit(in ExprOpHasAttr e) {
        visit(e.left);
        foreach (a; e.ap) {
            value = forceValue(value, e.loc);
            if (value.type == Type.Attrs) {
                auto j = getName(a) in value.attrs;
                if (j) {
                    value = *j;
                    continue;
                }
            }
            value = Value(false);
            return;
        }
        value = Value(true);
    }

    void visit(in ExprAttrs e) {
        const(Env)* dynamicEnv = env;
        Bindings attrs;
        if (e.recursive) {
            const hasOverrides = "__overrides" in e.attrs;

            // Create a new environment that contains the attributes in this `rec'.
            auto newEnv = Env(env, attrs);

            // The recursive attributes are evaluated in the new
            // environment, while the inherited attributes are evaluated
            // in the original environment.
            foreach (k, v; e.attrs) {
                if (hasOverrides && !v.inherited) {
                    attrs[k] = mkThunk(v.value, newEnv);
                } else {
                    attrs[k] = maybeThunk(v.value, v.inherited ? *env : newEnv);
                }
            }

            // If the rec contains an attribute called `__overrides', then
            // evaluate it, and add the attributes in that set to the rec.
            // This allows overriding of recursive attributes, which is
            // otherwise not possible.  (You can use the // operator to
            // replace an attribute, but other attributes in the rec will
            // still reference the original value, because that value has
            // been substituted into the bodies of the other attributes.
            // Hence we need __overrides.)
            if (hasOverrides) {
                auto vOverrides = forceAttrs(attrs["__overrides"], e.loc);
                Bindings newBnds;
                foreach (k,v; attrs) {
                    newBnds[k] = v;
                }
                foreach(k,v; vOverrides.attrs) {
                    // auto j = k in e.attrs;
                    newBnds[k] = v; // overwrites
                }
            }
            dynamicEnv = &newEnv;
        } else {
            foreach (k, v; e.attrs) {
                attrs[k] = maybeThunk(v.value, *env);
            }
        }
        // Dynamic attrs apply *after* rec and __overrides.
        foreach (v; e.dynamicAttrs) {
            visit(v.name);
            auto nameVal = forceValue(value, e.loc);
            if (nameVal.type == Type.Null) {
                continue;
            }
            assert(nameVal.type == Type.String, "a string was expected");
            const nameSym = nameVal.s;
            assert(nameSym !in attrs, "dynamic attribute already defined");
            attrs[nameSym] = maybeThunk(v.value, *dynamicEnv);
        }
        value = Value(attrs);
    }

    void visit(in ExprList e) {
        Value[] vars;
        foreach (v; e.elems)
            vars ~= maybeThunk(v, *env);
        value = Value(vars);
    }

    void visit(in ExprLambda e) {
        value = Value(e, *env);
    }

    void visit(in ExprLet e) {
        Bindings b;
        // Create a new environment that contains the attributes in this `let'.
        auto newEnv = Env(env, b);
        foreach (k, v; e.attrs.attrs) {
            b[k] = maybeThunk(v.value, v.inherited ? *env : newEnv);
        }
        env = &newEnv;
        visit(e.body);
        env = newEnv.up;
    }

    void visit(in ExprWith e) {
        debug writeln(e.attrs);
        auto att = visit(e.attrs);// TODO: make lazy
        assert(att.type == Type.Attrs, to!string(att.type) ~ ", expected attrs");
        const newEnv = Env(env, att.attrs);
        env = &newEnv;
        visit(e.body);
        env = newEnv.up;
    }

    void visit(in ExprIf e) {
        if (visit(e.cond).boolean)
            visit(e.then);
        else
            visit(e.else_);
    }

    void visit(in ExprAssert e) {
        assert(visit(e.cond).boolean);
        visit(e.body);
    }
}

public Value eval(in Expr e, in Env *env = &staticBaseEnv) /*pure*/ {
    auto ev = new Evaluator(env);
    e.accept(ev);
    return ev.value;
}

unittest {
    auto ZERO = new ExprInt(0);
    auto false_ = new ExprVar("false");
    auto true_ = new ExprVar("true");

    assert(eval(new ExprBinaryOp(Tok.SUB, ZERO, new ExprFloat(3))) == Value(-3.0));
    assert(eval(new ExprBinaryOp(Tok.AND, false_, true_)) == Value(false));
    assert(eval(new ExprBinaryOp(Tok.OR, false_, true_)) == Value(true));

    auto att = new ExprAttrs();
    att.attrs["a"] = AttrDef(new ExprString("ok"));
    att.attrs["true"] = AttrDef(true_, true);
    assert(eval(att) == Value(["a": Value("ok"), "true": Value(true)]));
    att.dynamicAttrs ~= DynamicAttrDef(new ExprString("b"), new ExprString("p"));
    assert(eval(att) == Value(["a": Value("ok"), "true": Value(true), "b": Value("p")]));
    att.recursive = true;
    att.attrs["c"] = AttrDef(new ExprVar("a"));
    att.dynamicAttrs ~= DynamicAttrDef(new ExprString("d"), new ExprVar("b"));
    // assert(eval(att) == Value(["a": Value("ok"), "true": Value(true), "b": Value("ok")])); needs `rec`
    assert(eval(new ExprFloat(1.1)) == Value(1.1));
    assert(eval(new ExprInt(42)) == Value(42));
    assert(eval(new ExprString("foo")) == Value("foo"));
    assert(eval(new ExprPath("fo/o")) == Value("fo/o"));
    assert(eval(false_) == Value(false));
    assert(eval(new ExprVar("null")) == Value());
    assert(eval(true_) == Value(true));
    assert(eval(new ExprAssert(true_, new ExprString("ok"))) == Value("ok"));
    assert(eval(new ExprOpNot(true_)) == Value(false));
    assert(eval(new ExprOpNot(false_)) == Value(true));
    assert(eval(new ExprIf(true_, new ExprString("ok"), new ExprFloat(1.1))) == Value("ok"));
    assert(eval(new ExprIf(false_, new ExprFloat(1.1), new ExprString("ok"))) == Value("ok"));
    assert(eval(new ExprList([new ExprString("ok")])) == Value([Value("ok")]));
}
