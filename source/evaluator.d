module nix.evaluator;

public import nix.value;
import nix.primops;

debug import std.stdio : writeln;

ref Value maybeThunk(in Expr e, Env* env) /*pure*/ {
    // debug writeln(__LINE__, env.vars);
    auto thunker = new Thunker(env);
    e.accept(thunker);
    return *thunker.value;
}

Value callFunction(ref Value fun, ref Value arg, in Loc pos) /*pure*/ {
    // debug writeln("callFunction ", fun, arg);
    auto f = forceValue(fun, pos);
    switch(f.type) {
    case Type.PrimOp:
        assert(0);
    case Type.PrimOpApp:
        return f.primOp()(f.primOpArgs ~ &arg);
    case Type.Attrs:
        auto functor = f.attrs["__functor"];
        auto v2 = callFunction(*functor, f, pos);
        return callFunction(v2, arg, pos);
    case Type.Lambda:
        auto env2 = new Env(f.env);
        if (!f.lambda.formals) {
            // add f.lambda.arg to the new env
            env2.vars[f.lambda.arg] = arg.dup;
        } else {
            auto attrs = forceValue(arg, pos).attrs;
            // For each formal argument, get the actual argument.  If
            // there is no matching actual argument but the formal
            // argument has a default, use the default.
            int attrsUsed;
            foreach (formal; f.lambda.formals.elems) {
                auto j = formal.name in attrs;
                if (!j) {
                    assert(formal.def, "Lambda called without required argument "~formal.name);
                    env2.vars[formal.name] = maybeThunk(formal.def, env2).dup;
                } else {
                    attrsUsed++;
                    env2.vars[formal.name] = *j;
                }
            }
            if (!f.lambda.formals.ellipsis && attrsUsed != attrs.length) {
                foreach (const k, ref v; attrs)
                    assert(0, "Lambda called with unexpected argument "~k);
                assert(0);
            }
        }
        return eval(f.lambda.body, *env2);
    default:
        assert(0, "attempt to call something which is not a function");
    }
}

ref Value forceValueDeep(ref Value value) {
    // debug writeln("forceValueDeep ", value);
    forceValue(value);
    switch (value.type) {
    case Type.Attrs:
        // TODO: detect infinite recursion
        foreach (v; value.attrs) {
            forceValueDeep(*v);
        }
        break;
    case Type.List:
        foreach (v; value.list) {
            forceValueDeep(*v);
        }
        break;
    // case Type.Ref:
    //     value = forceValueDeep(value.deref);
    //     break;
    default:
        break;
    }
    return value;
}

ref Value forceValue(ref Value value, in Loc pos = Loc()) {
    // debug writeln("forceValue ", value);
    switch (value.type) {
    // case Type.Ref:
    //     value = forceValue(value.deref);
    //     break;
    case Type.Thunk:
        auto thunk = value.thunk;
        auto env = value.env;
        value = Value(double.nan); // avoid infinite recursion
        // scope (failure)
        value = eval(thunk, *env);
        break;
    case Type.PrimOp:
        value = value.primOp()();
        assert(value.type != Type.Thunk);
        break;
    case Type.App:
        value = callFunction(*value.app[0], *value.app[1], pos);
        assert(value.type != Type.Thunk);
        break;
    // case Type.Blackhole:
        // assert(0, "infinite recursion encountered");
    default:
        break;
    }
    return value;
}

unittest {
    assert(eval(new ExprVar("__currentTime")).integer);
    assert(eval(new ExprVar("__typeOf")).primOpArgs);
    assert("int" == eval(new ExprBinaryOp(Tok.APP, new ExprVar("__typeOf"), new ExprInt(2))).str);
    assert(eval(new ExprVar("__add")).primOpArgs);
    assert(eval(new ExprBinaryOp(Tok.APP, new ExprVar("__add"), new ExprInt(2))).primOpArgs);
    assert(5 == eval(new ExprBinaryOp(Tok.APP, new ExprBinaryOp(Tok.APP, new ExprVar("__add"), new ExprInt(2)), new ExprInt(3))).integer);
}

string coerceToString(ref Value value, bool coerceMore = false) /*pure*/ {
    forceValue(value);
    switch (value.type) {
    case Type.String:
        return value.str;
    case Type.Path:
        // TODO: make absolute/canon?
        return value.path;
    case Type.Attrs:
        auto toString = "__toString" in value.attrs;
        if (toString) {
            auto str = callFunction(**toString, value, Loc());
            auto s = coerceToString(str, coerceMore);
            if (s) return s;
        }
        auto i = "outPath" in value.attrs;
        assert(i, "cannot coerce a set to a string");
        return coerceToString(**i, coerceMore);
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
            foreach(i, ref v; value.list) {
                if (i) result ~= ' ';
                result ~= coerceToString(value, coerceMore);
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
    auto nil = Value();
    assert("" == coerceToString(nil, true));
}

string coerceToPath(ref Value v) {
    const path = coerceToString(v, false);
    if (path == "" || path[0] != '/') throw new Error("string "~path~" doesn't represent an absolute path");
    return path;
}

private string canonicalPath(string p) @safe {
    import std.path : expandTilde, absolutePath, asNormalizedPath;
    return asNormalizedPath(absolutePath(expandTilde(p))).array;
}

private class Thunker : ConstVisitorT!(Expr, ExprNop, ExprInt, ExprFloat, ExprString, ExprPath, ExprVar) {
    Env *env;
    Value *value;

    this(Env* env) {
        assert(env);
        this.env = env;
    }

    Value* lookupVar(string name) {
        for (auto curEnv = this.env; curEnv; curEnv = curEnv.up) {
            // if (curEnv.hasWith) visit(curEnv.hasWith.attrs)
            auto v = name in curEnv.vars;
            if (v)
                return *v;
        }
        return null;
    }

    private void mkThunk(in Expr e) {
        value = new Value(e, env);
    }

    void visit(in Expr e) {
        mkThunk(e);
    }

    void visit(in ExprNop e) {
        e.expr.accept(this);
    }

    void visit(in ExprInt e) {
        value = new Value(e.n);
    }

    void visit(in ExprFloat e) {
        value = new Value(e.f);
    }

    void visit(in ExprString e) {
        value = new Value(e.s, null);
    }

    void visit(in ExprPath e) {
        value = new Value(canonicalPath(e.p));
    }

    void visit(in ExprVar e) {
        auto v = lookupVar(e.name);
        // The value might not be initialised in the environment yet.
        if (v) {
            value = v;
        } else {
            mkThunk(e);
        }
    }
}

unittest {
    auto thunker = new Thunker(new Env);
    new ExprInt(42).accept(thunker);
    assert(thunker.value.integer == 42);
    new ExprList([new ExprInt(42)]).accept(thunker);
    assert(thunker.value.thunk);
}

private bool eqValues(ref Value lhs, ref Value rhs) {
    auto l = forceValue(lhs);
    auto r = forceValue(rhs);

    if (l.type == Type.List && r.type == Type.List) {
        if (l.list.length != r.list.length) return false;
        foreach (i, v; l.list) {
            if (!eqValues(*v, *r.list[i])) return false;
        }
        return true;
    // } else if (l.type == Type.Attrs && r.type == Type.Attrs) {
    //     if (l.attrs.length != r.attrs.length) return false;
    } else {
        // Fallback to opEquals
        return l == r;
    }
}

class Evaluator : ConstVisitors {
    Env* env;
    Value *value;

    this(Env* env) /*pure*/ {
        assert(env);
        this.env = env;
    }

    private ref Value visit(in Expr expr) {
        assert(expr);//test
        if (expr)
            expr.accept(this);
        return *value;
    }

    void visit(in ExprNop expr) {
        visit(expr.expr);
    }

    void visit(in ExprOpNot expr) {
        value = new Value(!visit(expr.expr).boolean);
    }

    void visit(in ExprBinaryOp expr) {
        switch (expr.op) {
        case Tok.AND:
            if (visit(expr.left).boolean)
                visit(expr.right);
            assert(value.type == Type.Bool, "a boolean was expected");
            break;
        case Tok.OR:
            if (!visit(expr.left).boolean)
                visit(expr.right);
            assert(value.type == Type.Bool, "a boolean was expected");
            break;
        case Tok.EQ:
            auto lhs = visit(expr.left);
            auto rhs = visit(expr.right);
            value = new Value(eqValues(lhs, rhs));
            break;
        case Tok.NEQ:
            auto lhs = visit(expr.left);
            auto rhs = visit(expr.right);
            value = new Value(!eqValues(lhs, rhs));
            break;
        case Tok.CONCAT:
            auto lhs = visit(expr.left);
            auto rhs = visit(expr.right);
            value = new Value(forceValue(lhs).list ~ forceValue(rhs).list);
            break;
        case Tok.MUL:
            auto __mul = lookupVar("__mul"); // can be overridden
            auto lhs = visit(expr.left);
            auto partial = callFunction(__mul, lhs, expr.loc);
            auto rhs = visit(expr.right);
            value = callFunction(partial, rhs, expr.loc).dup;
            break;
        case Tok.DIV:
            auto __div = lookupVar("__div"); // can be overridden
            auto lhs = visit(expr.left);
            auto partial = callFunction(__div, lhs, expr.loc);
            auto rhs = visit(expr.right);
            value = callFunction(partial, rhs, expr.loc).dup;
            break;
        case Tok.ADD:
            // auto __add = lookupVar("__add"); // can be overridden
            // Strangely enough, + operator cannot currently be overridden
            auto lhs = visit(expr.left);
            auto rhs = visit(expr.right);
            value = (forceValue(lhs) + forceValue(rhs)).dup;
            break;
        case Tok.NEGATE:
        case Tok.SUB:
            auto __sub = lookupVar("__sub"); // can be overridden
            auto lhs = visit(expr.left);
            auto partial = callFunction(__sub, lhs, expr.loc);
            auto rhs = visit(expr.right);
            value = callFunction(partial, rhs, expr.loc).dup;
            break;
        case Tok.UPDATE:
            Bindings b;
            foreach (k, v; visit(expr.left).attrs) {
                b[k] = v;
            }
            foreach (k, v; visit(expr.right).attrs) {
                b[k] = v;
            }
            value = new Value(b);
            break;
        case Tok.LT:
            auto __lessThan = lookupVar("__lessThan"); // can be overridden
            auto lhs = visit(expr.left);
            auto partial = callFunction(__lessThan, lhs, expr.loc);
            auto rhs = visit(expr.right);
            value = callFunction(partial, rhs, expr.loc).dup;
            break;
        case Tok.LEQ:
            auto __lessThan = lookupVar("__lessThan"); // can be overridden
            auto rhs = visit(expr.right);
            auto partial = callFunction(__lessThan, rhs, expr.loc);
            auto lhs = visit(expr.left);
            const gt = callFunction(partial, lhs, expr.loc);
            value = new Value(!gt.boolean);
            break;
        case Tok.GT:
            auto __lessThan = lookupVar("__lessThan"); // can be overridden
            auto rhs = visit(expr.right);
            auto partial = callFunction(__lessThan, rhs, expr.loc);
            auto lhs = visit(expr.left);
            value = callFunction(partial, lhs, expr.loc).dup;
            break;
        case Tok.GEQ:
            auto __lessThan = lookupVar("__lessThan"); // can be overridden
            auto lhs = visit(expr.left);
            auto partial = callFunction(__lessThan, lhs, expr.loc);
            auto rhs = visit(expr.right);
            const lt = callFunction(partial, rhs, expr.loc);
            value = new Value(!lt.boolean);
            break;
        case Tok.IMPL:
            if (visit(expr.left).boolean)
                visit(expr.right);
            else
                value = new Value(true);
            assert(value.type == Type.Bool, "a boolean was expected");
            break;
        case Tok.APP:
            auto lhs = visit(expr.left);
            value = callFunction(lhs, maybeThunk(expr.right, env), expr.loc).dup;
            break;
        default:
            assert(0);
        }
    }

    ref Value lookupVar(string name) {
        for (auto curEnv = this.env; curEnv; curEnv = curEnv.up) {
            // if (!curEnv.vars) continue;
            // debug writeln("lookupVarx ", name, curEnv.vars);
            // if (curEnv.hasWith) visit(curEnv.hasWith.attrs)
            auto v = name in curEnv.vars;
            if (v)
                return **v;
        }
        throw new Error("undefined variable "~name);
    }

    void visit(in ExprInt expr) {
        value = new Value(expr.n);
    }

    void visit(in ExprFloat expr) {
        value = new Value(expr.f);
    }

    void visit(in ExprString expr) {
        value = new Value(expr.s, null);
    }

    void visit(in ExprPath expr) {
        value = new Value(canonicalPath(expr.p));
    }

    void visit(in ExprVar expr) {
        value = &forceValue(lookupVar(expr.name), expr.loc);
    }

    private string getName(in ref AttrName an) {
        if (an.ident) {
            return an.ident;
        } else {
            return forceValue(visit(an.expr)).str;
        }
    }

    void visit(in ExprSelect expr) {
        visit(expr.left);
        foreach (a; expr.ap) {
            forceValue(*value, expr.loc);
            const name = getName(a);
            if (value.type == Type.Attrs) {
                auto j = name in value.attrs;
                if (j) {
                    value = *j;
                    continue;
                }
            }
            if (expr.def) {
                visit(expr.def);
                break;
            }
            throw new Error("attribute missing: " ~ name);
        }
        forceValue(*value, Loc()); // TODO: use pos from attr 'j'
    }

    void visit(in ExprOpHasAttr expr) {
        visit(expr.left);
        foreach (a; expr.ap) {
            forceValue(*value, expr.loc);
            if (value.type == Type.Attrs) {
                auto j = getName(a) in value.attrs;
                if (j) {
                    value = *j;
                    continue;
                }
            }
            value = new Value(false);
            return;
        }
        value = new Value(true);
    }

    void visit(in ExprAttrs expr) {
        auto dynamicEnv = env;
        Bindings attrs = empty;
        if (expr.recursive) {
            const hasOverrides = "__overrides" in expr.attrs;

            // Create a new environment that contains the attributes in this `rec'.
            auto newEnv = new Env(env, attrs);

            // The recursive attributes are evaluated in the new
            // environment, while the inherited attributes are evaluated
            // in the original environment.
            foreach (k, ref v; expr.attrs) {
                if (hasOverrides && !v.inherited) {
                    attrs[k] = new Value(v.value, newEnv);
                } else {
                    attrs[k] = maybeThunk(v.value, v.inherited ? env : newEnv).dup;
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
                auto vOverrides = forceValue(*attrs["__overrides"], expr.loc).attrs;
                foreach(k, ref v; vOverrides) {
                    // auto j = k in expr.attrs;
                    attrs[k] = v; // overwrites
                }
            }
            dynamicEnv = newEnv;
            // assert(dynamicEnv.vars is attrs);
        } else {
            foreach (k, attr; expr.attrs) {
                attrs[k] = maybeThunk(attr.value, env).dup;
            }
        }
        // Dynamic attrs apply *after* rec and __overrides.
        foreach (attr; expr.dynamicAttrs) {
            auto nameVal = visit(attr.name);
            forceValue(nameVal, expr.loc);
            if (nameVal.type == Type.Null) {
                continue;
            }
            const nameSym = nameVal.str;
            assert(nameSym !in attrs, "dynamic attribute already defined");
            attrs[nameSym] = maybeThunk(attr.value, dynamicEnv).dup;
        }
        value = new Value(attrs);
    }

    void visit(in ExprList expr) {
        Value*[] vars;
        foreach (e; expr.elems)
            vars ~= maybeThunk(e, env).dup;
        value = new Value(vars);
    }

    void visit(in ExprLambda expr) {
        value = new Value(expr, env);
    }

    void visit(in ExprLet expr) {
        // Create a new environment that contains the attributes in this `let'.
        auto newEnv = new Env(env);
        foreach (k, attr; expr.attrs.attrs) {
            newEnv.vars[k] = maybeThunk(attr.value, attr.inherited ? env : newEnv).dup;
        }
        env = newEnv;
        visit(expr.body);
        env = newEnv.up;
    }

    void visit(in ExprWith expr) {
        // debug writeln(expr.attrs);
        auto att = visit(expr.attrs);// TODO: make lazy
        auto newEnv = new Env(env, att.attrs);
        env = newEnv;
        visit(expr.body);
        env = newEnv.up;
    }

    void visit(in ExprIf expr) {
        if (visit(expr.cond).boolean)
            visit(expr.then);
        else
            visit(expr.else_);
    }

    void visit(in ExprAssert expr) {
        assert(visit(expr.cond).boolean);
        visit(expr.body);
    }
}

public ref Value eval(in Expr expr, ref Env env = staticBaseEnv) /*pure*/ {
    assert(expr);
    if (auto v = expr in env.cache) {
        assert(0);
        //return *v;
    }
    auto ev = new Evaluator(&env);
    expr.accept(ev);
    // env.cache[expr] = ev.value;
    assert(ev.value.type != Type.Thunk);
    return *ev.value;//env.cache[expr];
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
