module nix.evaluator;

import nix.value, nix.primops;

debug import std.stdio : writeln;

Value maybeThunk(in Expr e, in Env* env) /*pure*/ {
    // debug writeln(__LINE__, env.vars);
    auto thunker = new Thunker(env);
    e.accept(thunker);
    return thunker.value;
}

private Value callPrimOp(Value fun, Value arg, in Loc pos) /*pure*/ {
    // Value p = fun;
    // while (p.type != Type.PrimOp) {
    //     p = p.app[0];
    // }
    return fun.primOp()(fun.primOpArgs ~ arg);
}

Value callFunction(Value fun, Value arg, in Loc pos) /*pure*/ {
    // debug writeln("callFunction ", fun, arg);
    auto f = forceValue(fun, pos);
    switch(f.type) {
    case Type.PrimOp:
        assert(0);
    case Type.PrimOpApp:
        return callPrimOp(f, arg, pos);
    case Type.Attrs:
        const functor = f.attrs["__functor"];
        const v2 = callFunction(functor, f, pos);
        return callFunction(v2, arg, pos);
    case Type.Lambda:
        Bindings b = empty;
        auto env2 = new Env(f.env, b);
        if (!f.lambda.formals) {
            // add f.lambda.arg to the new env
            b[f.lambda.arg] = arg;
        } else {
            const attrs = forceValue(arg, pos).attrs;
            // For each formal argument, get the actual argument.  If
            // there is no matching actual argument but the formal
            // argument has a default, use the default.
            int attrsUsed;
            foreach (formal; f.lambda.formals.elems) {
                const j = formal.name in attrs;
                if (!j) {
                    assert(formal.def, "Lambda called without required argument "~formal.name);
                    b[formal.name] = maybeThunk(formal.def, env2);
                } else {
                    attrsUsed++;
                    b[formal.name] = *j;
                }
            }
            if (!f.lambda.formals.ellipsis && attrsUsed != attrs.length) {
                foreach (const k, v; attrs)
                    assert(0, "Lambda called with unexpected argument "~k);
                assert(0);
            }
        }
        return eval(f.lambda.body, *env2);
    default:
        assert(0, "attempt to call something which is not a function");
    }
}

Value forceValueDeep(Value v) {
    // debug writeln("forceValueDeep ", v);
    switch (v.type) {
    case Type.Attrs:
        // TODO: detect infinite recursion
        Bindings b;
        foreach (k, e; v.attrs) {
            // TODO: minimize memory allocation by using COW
            b[k] = forceValueDeep(e);
        }
        return Value(b);
    case Type.List:
        Value[] l;
        foreach (ref e; v.list) {
            // TODO: minimize memory allocation by using COW
            l ~= forceValueDeep(e);
        }
        return Value(l);
    default:
        return forceValue(v);
    }
}

Value forceValue(Value v, in Loc pos = Loc()) {
    // debug writeln("forceValue ", v);
    switch (v.type) {
    case Type.Thunk:
        return eval(v.thunk, *v.env);
    case Type.PrimOp:
        return v.primOp()();
    case Type.App:
        return callFunction(v.app[0], v.app[1], pos);
    // case Type.Blackhole:
        // assert(0, "infinite recursion encountered");
    default:
        return v;
    }
}

unittest {
    assert(eval(new ExprVar("__currentTime")).integer);
    assert(eval(new ExprVar("__typeOf")).primOpArgs);
    assert("int" == eval(new ExprBinaryOp(Tok.APP, new ExprVar("__typeOf"), new ExprInt(2))).str);
    assert(eval(new ExprVar("__add")).primOpArgs);
    assert(eval(new ExprBinaryOp(Tok.APP, new ExprVar("__add"), new ExprInt(2))).primOpArgs);
    assert(5 == eval(new ExprBinaryOp(Tok.APP, new ExprBinaryOp(Tok.APP, new ExprVar("__add"), new ExprInt(2)), new ExprInt(3))).integer);
}

string coerceToString(Value v, bool coerceMore = false) /*pure*/ {
    auto value = forceValue(v);
    switch (value.type) {
    case Type.String:
        return value.str;
    case Type.Path:
        // TODO: make absolute/canon?
        return value.path;
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
        break;
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
            break;
        }
    }
    assert(0, "cannot coerce "~value.typeOf~" to a string");
}

unittest {
    assert("" == coerceToString(Value(), true));
}

string coerceToPath(Value v) {
    const path = coerceToString(v, false);
    if (path == "" || path[0] != '/') throw new Error("string "~path~" doesn't represent an absolute path");
    return path;
}

private string canonicalPath(string p) @safe {
    import std.path : expandTilde, absolutePath, asNormalizedPath;
    return asNormalizedPath(absolutePath(expandTilde(p))).array;
}

private class Thunker : Visitor {
    const(Env) *env;
    Value value;

    this(in Env* env) {
        assert(env);
        this.env = env;
    }

    const(Value*) lookupVar(string name) {
        for (auto curEnv = this.env; curEnv; curEnv = curEnv.up) {
            // if (curEnv.hasWith) visit(curEnv.hasWith.attrs)
            auto v = name in curEnv.vars;
            if (v)
                return v;
        }
        return null;
    }


    private void mkThunk(in Expr e) {
        value = Value(e, env);
    }

    void visit(in ExprNop e) {
        e.expr.accept(this);
    }

    void visit(in ExprOpNot e) {
        mkThunk(e);
    }

    void visit(in ExprBinaryOp e) {
        mkThunk(e);
    }

    void visit(in ExprInt e) {
        value = Value(e.n);
    }

    void visit(in ExprFloat e) {
        value = Value(e.f);
    }

    void visit(in ExprString e) {
        value = Value(e.s, null);
    }

    void visit(in ExprPath e) {
        value = Value(canonicalPath(e.p));
    }

    void visit(in ExprVar e) {
        const v = lookupVar(e.name);
        // The value might not be initialised in the environment yet.
        if (v) {
            value = *v;
        } else {
            mkThunk(e);
        }
    }

    void visit(in ExprSelect e) {
        mkThunk(e);
    }

    void visit(in ExprOpHasAttr e) {
        mkThunk(e);
    }

    void visit(in ExprAttrs e) {
        mkThunk(e);
    }

    void visit(in ExprList e) {
        mkThunk(e);
    }

    void visit(in ExprLambda e) {
        mkThunk(e);
    }

    void visit(in ExprLet e) {
        mkThunk(e);
    }

    void visit(in ExprWith e) {
        mkThunk(e);
    }

    void visit(in ExprIf e) {
        mkThunk(e);
    }

    void visit(in ExprAssert e) {
        mkThunk(e);
    }
}

private bool eqValues(Value lhs, Value rhs) {
    auto l = forceValue(lhs);
    auto r = forceValue(rhs);

    if (l.type == Type.List && r.type == Type.List) {
        if (l.list.length != r.list.length) return false;
        foreach (i, ref v; l.list) {
            if (!eqValues(v, r.list[i])) return false;
        }
        return true;
    // } else if (l.type == Type.Attrs && r.type == Type.Attrs) {
    //     if (l.attrs.length != r.attrs.length) return false;
    } else {
        // Fallback to opEquals
        return l == r;
    }
}

class Evaluator : Visitor {
    const(Env)* env;
    Value value;

    this(in Env* env) /*pure*/ {
        assert(env);
        this.env = env;
    }

    private Value visit(in Expr e) {
        if (e)
            e.accept(this);
        return value;
    }

    void visit(in ExprNop e) {
        visit(e.expr);
    }

    void visit(in ExprOpNot e) {
        visit(e.expr);
        value = Value(!value.boolean);
    }

    void visit(in ExprBinaryOp e) {
        switch (e.op) {
        case Tok.AND:
            if (visit(e.left).boolean)
                visit(e.right);
            assert(value.type == Type.Bool, "a boolean was expected");
            break;
        case Tok.OR:
            if (!visit(e.left).boolean)
                visit(e.right);
            assert(value.type == Type.Bool, "a boolean was expected");
            break;
        case Tok.EQ:
            value = Value(eqValues(visit(e.left), visit(e.right)));
            break;
        case Tok.NEQ:
            value = Value(!eqValues(visit(e.left), visit(e.right)));
            break;
        case Tok.CONCAT:
            value = Value(forceValue(visit(e.left)).list ~ forceValue(visit(e.right)).list);
            break;
        case Tok.MUL:
            auto __mul = lookupVar("__mul"); // can be overridden
            value = callFunction(callFunction(__mul, visit(e.left), e.loc), visit(e.right), e.loc);
            break;
        case Tok.DIV:
            auto __div = lookupVar("__div"); // can be overridden
            value = callFunction(callFunction(__div, visit(e.left), e.loc), visit(e.right), e.loc);
            break;
        case Tok.ADD:
            // Strangely enough, cannot be overridden
            value = forceValue(visit(e.left)) + forceValue(visit(e.right));
            break;
        case Tok.NEGATE:
        case Tok.SUB:
            auto __sub = lookupVar("__sub"); // can be overridden
            value = callFunction(callFunction(__sub, visit(e.left), e.loc), visit(e.right), e.loc);
            break;
        case Tok.UPDATE:
            Bindings b;
            foreach (k, v; visit(e.left).attrs) {
                b[k] = v;
            }
            foreach (k, v; visit(e.right).attrs) {
                b[k] = v;
            }
            value = Value(b);
            break;
        case Tok.LT:
            auto __lessThan = lookupVar("__lessThan"); // can be overridden
            value = callFunction(callFunction(__lessThan, visit(e.left), e.loc), visit(e.right), e.loc);
            break;
        case Tok.LEQ:
            auto __lessThan = lookupVar("__lessThan"); // can be overridden
            const gt = callFunction(callFunction(__lessThan, visit(e.right), e.loc), visit(e.left), e.loc);
            value = Value(!gt.boolean);
            break;
        case Tok.GT:
            auto __lessThan = lookupVar("__lessThan"); // can be overridden
            value = callFunction(callFunction(__lessThan, visit(e.right), e.loc), visit(e.left), e.loc);
            break;
        case Tok.GEQ:
            auto __lessThan = lookupVar("__lessThan"); // can be overridden
            const lt = callFunction(callFunction(__lessThan, visit(e.left), e.loc), visit(e.right), e.loc);
            value = Value(!lt.boolean);
            break;
        case Tok.IMPL:
            if (visit(e.left).boolean)
                visit(e.right);
            else
                value = Value.TRUE;
            assert(value.type == Type.Bool, "a boolean was expected");
            break;
        case Tok.APP:
            value = callFunction(visit(e.left), maybeThunk(e.right, env), e.loc);
            break;
        default:
            assert(0);
        }
    }

    const(Value) lookupVar(string name) {
        for (auto curEnv = this.env; curEnv; curEnv = curEnv.up) {
            // if (!curEnv.vars) continue;
            // debug writeln("lookupVarx ", name, curEnv.vars);
            // if (curEnv.hasWith) visit(curEnv.hasWith.attrs)
            auto v = name in curEnv.vars;
            if (v)
                return *v;
        }
        throw new Error("undefined variable "~name);
    }

    void visit(in ExprInt e) {
        value = Value(e.n);
    }

    void visit(in ExprFloat e) {
        value = Value(e.f);
    }

    void visit(in ExprString e) {
        value = Value(e.s, null);
    }

    void visit(in ExprPath e) {
        value = Value(canonicalPath(e.p));
    }

    void visit(in ExprVar e) {
        value = forceValue(lookupVar(e.name), e.loc);
    }

    string getName(in ref AttrName an) {
        if (an.ident) {
            return an.ident;
        } else {
            visit(an.expr);
            return forceValue(value).str;
        }
    }

    void visit(in ExprSelect e) {
        visit(e.left);
        foreach (a; e.ap) {
            value = forceValue(value);
            if (value.type == Type.Attrs) {
                const j = getName(a) in value.attrs;
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
        Bindings attrs = empty;
        if (e.recursive) {
            const hasOverrides = "__overrides" in e.attrs;

            // Create a new environment that contains the attributes in this `rec'.
            auto newEnv = new Env(env, attrs);

            // The recursive attributes are evaluated in the new
            // environment, while the inherited attributes are evaluated
            // in the original environment.
            foreach (k, v; e.attrs) {
                if (hasOverrides && !v.inherited) {
                    attrs[k] = Value(v.value, newEnv);
                } else {
                    attrs[k] = maybeThunk(v.value, v.inherited ? env : newEnv);
                }
            }
            // debug writeln(__LINE__, newEnv.vars);

            // If the rec contains an attribute called `__overrides', then
            // evaluate it, and add the attributes in that set to the rec.
            // This allows overriding of recursive attributes, which is
            // otherwise not possible.  (You can use the // operator to
            // replace an attribute, but other attributes in the rec will
            // still reference the original value, because that value has
            // been substituted into the bodies of the other attributes.
            // Hence we need __overrides.)
            if (hasOverrides) {
                auto vOverrides = forceValue(attrs["__overrides"], e.loc).attrs;
                Bindings newBnds;
                foreach (k,v; attrs) {
                    newBnds[k] = v;
                }
                foreach(k,v; vOverrides) {
                    // auto j = k in e.attrs;
                    newBnds[k] = v; // overwrites
                }
            }
            // *newEnv = Env(env, attrs);
            dynamicEnv = newEnv;
            const x = attrs;
            assert(dynamicEnv.vars == x);
        } else {
            foreach (k, v; e.attrs) {
                attrs[k] = maybeThunk(v.value, env);
            }
        }
        // Dynamic attrs apply *after* rec and __overrides.
        foreach (v; e.dynamicAttrs) {
            visit(v.name);
            const nameVal = forceValue(value, e.loc);
            if (nameVal.type == Type.Null) {
                continue;
            }
            const nameSym = nameVal.str;
            assert(nameSym !in attrs, "dynamic attribute already defined");
            attrs[nameSym] = maybeThunk(v.value, dynamicEnv);
        }
        // debug writeln(__LINE__, attrs);
        value = Value(attrs);
    }

    void visit(in ExprList e) {
        Value[] vars;
        foreach (v; e.elems)
            vars ~= maybeThunk(v, env);
        value = Value(vars);
    }

    void visit(in ExprLambda e) {
        value = Value(e, env);
    }

    void visit(in ExprLet e) {
        Bindings b = empty;
        // Create a new environment that contains the attributes in this `let'.
        auto newEnv = new Env(env, b);
        foreach (k, v; e.attrs.attrs) {
            b[k] = maybeThunk(v.value, v.inherited ? env : newEnv);
        }
        env = newEnv;
        visit(e.body);
        env = newEnv.up;
    }

    void visit(in ExprWith e) {
        // debug writeln(e.attrs);
        auto att = visit(e.attrs);// TODO: make lazy
        const newEnv = new Env(env, att.attrs);
        env = newEnv;
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

public Value eval(in Expr e, ref in Env env = staticBaseEnv) /*pure*/ {
    assert(e);
    auto ev = new Evaluator(&env);
    e.accept(ev);
    return ev.value;
}

unittest {
    auto ZERO = new ExprInt(0);
    auto false_ = new ExprVar("false");
    auto true_ = new ExprVar("true");
    auto ok = Value("ok", null);

    assert(eval(new ExprBinaryOp(Tok.SUB, ZERO, new ExprFloat(3))) == Value(-3.0));
    assert(eval(new ExprBinaryOp(Tok.AND, false_, true_)) == Value(false));
    assert(eval(new ExprBinaryOp(Tok.OR, false_, true_)) == Value(true));

    auto att = new ExprAttrs();
    // { a = "ok"; }
    att.attrs["a"] = ExprAttrs.AttrDef(new ExprString("ok"));
    assert(eval(att) == Value(["a": ok]));
    // { a = "ok"; true = true; }
    att.attrs["true"] = ExprAttrs.AttrDef(true_, true);
    assert(eval(att) == Value(["a": ok, "true": Value(true)]));
    // { a = "ok"; true = true; b = "p"; }
    att.dynamicAttrs ~= ExprAttrs.DynamicAttrDef(new ExprString("b"), new ExprString("p"));
    assert(eval(att) == Value(["a": ok, "true": Value(true), "b": Value("p", null)]));
    // rec { a = "ok"; true = true; b = "p"; }
    att.recursive = true;
    // rec { a = "ok"; true = true; b = "p"; c = a; }
    att.attrs["c"] = ExprAttrs.AttrDef(new ExprVar("a"));
    assert(eval(att).forceValueDeep == Value(["a": ok, "true": Value(true), "b": Value("p", null), "c": ok])); //needs `rec`
    // rec { a = "ok"; true = true; b = "p"; c = a; d = b; }
    att.dynamicAttrs ~= ExprAttrs.DynamicAttrDef(new ExprString("d"), new ExprVar("b"));
    assert(eval(att).forceValueDeep == Value(["a": ok, "true": Value(true), "b": Value("p", null), "c": ok, "d": Value("p", null)])); //needs `rec`
    assert(eval(new ExprFloat(1.1)) == Value(1.1));
    assert(eval(new ExprInt(42)) == Value(42));
    assert(eval(new ExprString("foo")) == Value("foo", null));
    assert(eval(new ExprPath("/fo/o")) == Value("/fo/o"));
    assert(eval(false_) == Value(false));
    assert(eval(new ExprVar("null")) == Value());
    assert(eval(true_) == Value(true));
    assert(eval(new ExprAssert(true_, new ExprString("ok"))) == ok);
    assert(eval(new ExprOpNot(true_)) == Value(false));
    assert(eval(new ExprOpNot(false_)) == Value(true));
    assert(eval(new ExprIf(true_, new ExprString("ok"), new ExprFloat(1.1))) == ok);
    assert(eval(new ExprIf(false_, new ExprFloat(1.1), new ExprString("ok"))) == ok);
    assert(eval(new ExprList([new ExprString("ok")])) == Value([ok]));
}
