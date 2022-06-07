module nix.evaluator;

public import nix.value;
import nix.primops : createBaseEnv;
import nix.visitor : ConstVisitorT;
import nix.path;

debug import std.stdio : writeln;
import std.exception : enforce;

class AssertionException : EvalException {
    this(string msg, string file = __FILE__, size_t line = __LINE__) pure {
        super("assertion "~msg~" failed", file, line);
    }
}

ref Value maybeThunk(in Expr e, Env* env) /*pure*/ {
    // debug(EVAL) writeln(__LINE__, env.vars);
    auto thunker = new Thunker(env);
    e.accept(thunker);
    return *thunker.value;
}

Value callFunction(ref Value fun, ref Value arg) /*pure*/ {
    // debug(EVAL) writeln("callFunction ", fun, arg);
    auto f = forceValue(fun);
    switch(f.type) {
    case Type.PrimOp:
        assert(0);
    case Type.PrimOpApp:
        return f.primOp()(f.primOpArgs ~ &arg);
    case Type.Attrs:
        auto functor = f.attrs["__functor"];
        auto v2 = callFunction(*functor, f);
        return callFunction(v2, arg);
    case Type.Lambda:
        auto env2 = new Env(f.env);
        if (f.lambda.arg) {
            // add f.lambda.arg to the new env
            env2.vars[f.lambda.arg] = Attr(arg.dup);
        }
        if (f.lambda.formals) {
            auto attrs = forceValue(arg).attrs;
            // For each formal argument, get the actual argument.  If
            // there is no matching actual argument but the formal
            // argument has a default, use the default.
            int attrsUsed;
            foreach (formal; f.lambda.formals.elems) {
                if (auto j = formal.name in attrs) {
                    attrsUsed++;
                    env2.vars[formal.name] = *j;
                } else {
                    enforce!TypeException(formal.def, "Lambda called without required argument "~formal.name);
                    env2.vars[formal.name] = Attr(new Value(formal.def, env2));
                }
            }
            if (!f.lambda.formals.ellipsis && attrsUsed != attrs.length) {
                foreach (const k, ref v; attrs)
                    throw new TypeException("Lambda called with unexpected argument "~k);
                assert(0);
            }
        }
        return eval(f.lambda.body, *env2);
    default:
        throw new TypeException("attempt to call something which is not a function");
    }
}

ref Value forceValueDeep(return ref Value value) {
    // debug(EVAL) writeln("forceValueDeep ", value);
    forceValue(value);
    switch (value.type) {
    case Type.Attrs:
        // TODO: detect infinite recursion
        foreach (k, v; value.attrs) {
            scope(failure) writeln("while evaluating the attribute ", k);
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

ref Value forceValue(return ref Value value) {
    debug(EVAL) writeln("forceValue ", value);
    switch (value.type) {
    // case Type.Ref:
    //     value = forceValue(value.deref);
    //     break;
    case Type.Thunk:
        auto expr = value.thunk;
        auto env = value.env;
        value.type = Type.Blackhole; // avoid infinite recursion
        value = eval(expr, *env);
        break;
    case Type.PrimOp:
        value = value.primOp()();
        assert(value.type != Type.Thunk);
        break;
    case Type.App:
        value = callFunction(*value.app[0], *value.app[1]);
        assert(value.type != Type.Thunk);
        break;
    case Type.Blackhole:
        throw new EvalException("infinite recursion encountered");
    default:
        break;

    }
    return value;
}

unittest {
    assert(eval(new ExprVar(LOC, "__currentTime")).integer);
    assert(eval(new ExprVar(LOC, "__typeOf")).primOpArgs);
    assert("int" == eval(new ExprBinaryOp(LOC, Tok.APP, new ExprVar(LOC, "__typeOf"), new ExprInt(LOC, 2))).str);
    assert(eval(new ExprVar(LOC, "__add")).primOpArgs);
    assert(eval(new ExprBinaryOp(LOC, Tok.APP, new ExprVar(LOC, "__add"), new ExprInt(LOC, 2))).primOpArgs);
    assert(5 == eval(new ExprBinaryOp(LOC, Tok.APP, new ExprBinaryOp(LOC, Tok.APP, new ExprVar(LOC, "__add"), new ExprInt(LOC, 2)), new ExprInt(LOC, 3))).integer);
}

string forceStringNoCtx(ref Value str) {
    forceValue(str);
    enforce!EvalException(!str.context.length, "the string '"~str.str~"' is not allowed to refer to a store path");
    return str.str;
}

String tryAttrsToString(ref Value value, bool coerceMore, bool copyToStore) {
    auto toString = "__toString" in value.attrs;
    if (!toString) return String.init;
    auto str = callFunction(**toString, value);
    return coerceToString(str, coerceMore, copyToStore);
}

String coerceToString(ref Value value, bool coerceMore = false, bool copyToStore = true, bool canonicalize = true) {
    forceValue(value);
    switch (value.type) {
    case Type.String:
        return value.str;
    case Type.Path:
        auto path = canonicalize ? canonPath(value.path) : value.path;
        return copyToStore ? copyPathToStore(path) : String(path);
    case Type.Attrs:
        auto s = tryAttrsToString(value, coerceMore, copyToStore);
        if (s !is null) return s;
        if (auto outPath = "outPath" in value.attrs) {
            return coerceToString(**outPath, coerceMore, copyToStore);
        }
        break;
    // case Type.External: TODO
    default:
        if (!coerceMore) break;
        switch (value.type) {
        case Type.Bool:
            return String(value.boolean ? "1" : "");
        case Type.Null:
            return String("");
        case Type.List:
            String result;
            foreach(i, v; value.list) {
                if (i) result.raw ~= ' ';
                result.raw ~= coerceToString(*v, coerceMore, copyToStore);
                // TODO: merge contexts
            }
            return result;
        case Type.Int:
        case Type.Float:
            return String(value.toString());
        default:
        }
    }
    throw new TypeException("cannot coerce "~value.typeOf~" to a string");
}

unittest {
    auto nil = Value();
    assert("" == coerceToString(nil, true));
}

string coerceToPath(ref Value v) {
    const path = coerceToString(v, false, false);
    enforce!EvalException(path != "" && path[0] == '/', "string '"~path~"' doesn't represent an absolute path");
    // FIXME: don't discard context
    return path;
}

private String copyPathToStore(Path path) {
    import nix.path: isDerivation;
    enforce!EvalException(!isDerivation(path), "file names are not allowed to end in '"~drvExtension~"'");
    auto dstPath = printStorePath(computeStorePathForPath(baseNameOf(path), path));
    return String(dstPath, [dstPath:true]);
}

private Path computeStorePathForPath(string name, Path path) @safe pure {
    // FIXME: implement file hashing
    return "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz-" ~ name;
}

private class Thunker : ConstVisitorT!(Expr, ExprInt, ExprFloat, ExprString, ExprPath, ExprVar) {
    Env *env;
    Value *value;

    this(Env* env) {
        assert(env);
        this.env = env;
    }

    Value* lookupVar(string name) {
        for (auto curEnv = this.env; curEnv; curEnv = curEnv.up) {
            if (auto v = name in curEnv.vars) {
                return *v;
            }
        }
        return null;
    }

    private void mkThunk(in Expr e) {
        value = new Value(e, env);
    }

    override void visit(in Expr e) {
        mkThunk(e);
    }

    override void visit(in ExprInt e) {
        value = new Value(e.n);
    }

    override void visit(in ExprFloat e) {
        value = new Value(e.f);
    }

    override void visit(in ExprString e) {
        value = new Value(e.s, null);
    }

    override void visit(in ExprPath e) {
        value = new Value(absPath(e.p));
    }

    override void visit(in ExprVar e) {
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
    new ExprInt(LOC, 42).accept(thunker);
    assert(thunker.value.integer == 42);
    new ExprList(LOC, [new ExprInt(LOC, 42)]).accept(thunker);
    assert(thunker.value.thunk);
}

private bool isDerivation(ref Value v) {
    if (v.type != Type.Attrs) return false;
    auto type = "type" in v.attrs;
    return type && forceValue(**type).type == Type.String && (*type).str == "derivation";
}

bool eqValues(ref Value lhs, ref Value rhs) {
    auto l = forceValue(lhs);
    auto r = forceValue(rhs);

    if (l.type == Type.List && r.type == Type.List) {
        if (l.list.length != r.list.length) return false;
        foreach (i, v; l.list) {
            if (!eqValues(*v, *r.list[i])) return false;
        }
        return true;
    } else if (l.type == Type.Attrs && r.type == Type.Attrs) {
        /* If both sets denote a derivation (type = "derivation"),
               then compare their outPaths. */
        if (l.isDerivation && r.isDerivation) {
            return eqValues(*l.attrs["outPath"], *r.attrs["outPath"]);
        }
        if (l.attrs.length != r.attrs.length) return false;
        foreach (k, p; l.attrs) {
            auto pp = k in r.attrs;
            debug(EVAL) writeln("comparing ", k, ": ", p, " vs ", pp);
            if (pp is null || !eqValues(*p, **pp)) return false;
        }
        return true;
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
        expr.accept(this);
        return *value;
    }

    void visit(in ExprOpNot expr) {
        scope(failure) writeln("while evaluating the expression ", expr);
        value = new Value(!visit(expr.expr).boolean);
    }

    void visit(in ExprBinaryOp expr) {
        scope(failure) writeln("while evaluating the expression ", expr);
        switch (expr.op) {
        case Tok.AND:
            if (visit(expr.left).boolean)
                visit(expr.right);
            enforce!TypeException(value.type == Type.Bool, "a boolean was expected");
            break;
        case Tok.OR:
            if (!visit(expr.left).boolean)
                visit(expr.right);
            enforce!TypeException(value.type == Type.Bool, "a boolean was expected");
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
            auto partial = callFunction(__mul, lhs);
            auto rhs = visit(expr.right);
            value = callFunction(partial, rhs).dup;
            break;
        case Tok.DIV:
            auto __div = lookupVar("__div"); // can be overridden
            auto lhs = visit(expr.left);
            auto partial = callFunction(__div, lhs);
            auto rhs = visit(expr.right);
            value = callFunction(partial, rhs).dup;
            break;
        case Tok.ADD:
            auto lhs = visit(expr.left);
            auto rhs = visit(expr.right);
            if (forceValue(lhs).isNumber) {
                // Strangely enough, + operator cannot currently be overridden
                // auto __add = lookupVar("__add"); // can be overridden
                value = (lhs + forceValue(rhs)).dup;
            } else {
                // TODO: skip canonization of first path
                auto str = coerceToString(rhs, false, lhs.type == Type.String);
                value = (lhs + Value(str, str.context)).dup;
            }
            break;
        case Tok.NEGATE:
        case Tok.SUB:
            auto __sub = lookupVar("__sub"); // can be overridden
            auto lhs = visit(expr.left);
            auto partial = callFunction(__sub, lhs);
            auto rhs = visit(expr.right);
            value = callFunction(partial, rhs).dup;
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
            auto partial = callFunction(__lessThan, lhs);
            auto rhs = visit(expr.right);
            value = callFunction(partial, rhs).dup;
            break;
        case Tok.LEQ:
            auto __lessThan = lookupVar("__lessThan"); // can be overridden
            auto rhs = visit(expr.right);
            auto partial = callFunction(__lessThan, rhs);
            auto lhs = visit(expr.left);
            const gt = callFunction(partial, lhs);
            value = new Value(!gt.boolean);
            break;
        case Tok.GT:
            auto __lessThan = lookupVar("__lessThan"); // can be overridden
            auto rhs = visit(expr.right);
            auto partial = callFunction(__lessThan, rhs);
            auto lhs = visit(expr.left);
            value = callFunction(partial, lhs).dup;
            break;
        case Tok.GEQ:
            auto __lessThan = lookupVar("__lessThan"); // can be overridden
            auto lhs = visit(expr.left);
            auto partial = callFunction(__lessThan, lhs);
            auto rhs = visit(expr.right);
            const lt = callFunction(partial, rhs);
            value = new Value(!lt.boolean);
            break;
        case Tok.IMPL:
            if (visit(expr.left).boolean)
                visit(expr.right);
            else
                value = new Value(true);
            enforce!TypeException(value.type == Type.Bool, "a boolean was expected");
            break;
        case Tok.APP:
            auto lhs = visit(expr.left);
            // TODO: detect tail recursion
            value = callFunction(lhs, maybeThunk(expr.right, env)).dup;
            break;
        default:
            assert(0);
        }
    }

    ref Value lookupVar(string name) {
        /* Check whether the variable appears in the environment. */
        for (auto curEnv = this.env; curEnv; curEnv = curEnv.up) {
            debug(EVAL) writeln("lookupVar ", name, " in env ", curEnv.vars);
            if (auto v = name in curEnv.vars) {
                // fromWith = false;
                return **v;
            }
        }
        /* Otherwise, the variable must be obtained from the nearest
        enclosing `with'.  If there is no `with', then we can issue an
        "undefined variable" error now. */
        for (auto curEnv = this.env; curEnv; curEnv = curEnv.up) {
            debug(EVAL) writeln("lookupVar(with) ", name, curEnv.withVars);
            if (curEnv.hasWith && !curEnv.withVars) {
                curEnv.withVars = eval(curEnv.hasWith, *curEnv.up).attrs;
            }
            if (auto v = name in curEnv.withVars) {
                // fromWith = true;
                return **v;
            }
        }
        throw new UndefinedVarException(name);
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
        value = new Value(absPath(expr.p));
    }

    void visit(in ExprVar expr) {
        scope(failure) writeln("while evaluating the variable ", expr);
        value = &forceValue(lookupVar(expr.name));
    }

    private string getName(in ref AttrName an) {
        if (an.ident) {
            return an.ident;
        } else {
            return forceStringNoCtx(eval(an.expr, *env));
        }
    }

    void visit(in ExprSelect expr) {
        scope(failure) writeln("while evaluating the select expression ", expr);
        visit(expr.left);
        debug(EVAL) writeln("after expr.left=",*value, " for ", expr.ap);
        foreach (a; expr.ap) {
            scope(failure) writeln("while evaluating the attribute ", a, " on ", expr.loc);
            const name = getName(a);
            forceValue(*value); // TODO: use pos from attr 'a'
            debug(EVAL) writeln("look for ", name, " in ", *value);
            if (value.type == Type.Attrs) {
                if (auto j = name in value.attrs) {
                    debug(EVAL) writeln(" found ", name, ": ", **j, (*j).type == Type.Thunk ? (*j).env.vars : null);
                    value = *j;
                    continue;
                }
            }
            if (expr.def) {
                debug(EVAL) writeln("defaulting to ", expr.def);
                visit(expr.def);
                break;
            }
            import nix.printer : format;
            debug(EVAL) writeln(format(expr), expr.ap, "=", *value);
            throw new EvalException("attribute '"~name~"' missing");
        }
        scope(failure) writeln("while evaluating the attribute path ", expr.ap.toString(), " on ", expr.loc);
        forceValue(*value);
    }

    void visit(in ExprOpHasAttr expr) {
        scope(failure) writeln("while evaluating the expression ", expr);
        visit(expr.left);
        foreach (a; expr.ap) {
            scope(failure) writeln("while evaluating the attribute ", a);
            forceValue(*value);
            if (value.type == Type.Attrs) {
                if (auto j = getName(a) in value.attrs) {
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
        scope(failure) writeln("while evaluating the expression ", expr);
        auto dynamicEnv = env;
        Bindings attrs = empty;
        if (expr.recursive) {
            debug(EVAL) writeln("ExprAttrs (expr.recursive)");
            const hasOverrides = "__overrides" in expr.attrs;

            // Create a new environment that contains the attributes in this `rec'.
            auto newEnv = new Env(env, attrs);

            // The recursive attributes are evaluated in the new
            // environment, while the inherited attributes are evaluated
            // in the original environment.
            foreach (k, v; expr.attrs) {
                debug(EVAL) writeln("rec ", k, " in env ", (v.inherited ? env : newEnv).vars);
                if (hasOverrides && !v.inherited) {
                    attrs[k] = Attr(new Value(v.value, newEnv));
                    assert(k in newEnv.vars);
                } else {
                    attrs[k] = Attr(maybeThunk(v.value, v.inherited ? env : newEnv).dup);
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
                auto vOverrides = forceValue(*attrs["__overrides"]).attrs;
                foreach(k, ref v; vOverrides) {
                    // auto j = k in expr.attrs;
                    attrs[k] = Attr(v); // overwrites
                }
            }
            dynamicEnv = newEnv;
            assert(dynamicEnv.vars is attrs);
            debug(EVAL) writeln("dynamicEnv is ", dynamicEnv.vars);
        } else {
            foreach (k, v; expr.attrs) {
                // attrs[k] = new Value(v.value, env);
                attrs[k] = Attr(maybeThunk(v.value, env).dup);
            }
        }
        // Dynamic attrs apply *after* rec and __overrides.
        foreach (attr; expr.dynamicAttrs) {
            debug(EVAL) writeln("dynamicAttr ", attr.name);
            env = dynamicEnv;
            auto nameVal = visit(attr.name);
            debug(EVAL) writeln("dynamicAttr ", nameVal);
            env = dynamicEnv.up;
            forceValue(nameVal);
            if (nameVal.type == Type.Null) {
                continue;
            }
            const nameSym = forceStringNoCtx(nameVal);
            enforce!EvalException(nameSym !in attrs, "dynamic attribute already defined: "~nameSym);
            attrs[nameSym] = Attr(maybeThunk(attr.value, dynamicEnv).dup);
        }
        debug(EVAL) foreach (k, v; attrs) {
            writeln("  ", k, " = ", *v, " in env ", v.type == Type.Thunk ? v.env.vars : cast(Bindings) null);
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
        scope(failure) writeln("while evaluating the expression ", expr);
        // Create a new environment that contains the attributes in this `let'.
        auto newEnv = new Env(env);
        // The recursive attributes are evaluated in the new environment,
        // while the inherited attributes are evaluated in the original
        // environment.
        foreach (k, attr; expr.attrs.attrs) {
            newEnv.vars[k] = Attr(maybeThunk(attr.value, attr.inherited ? env : newEnv).dup);
        }
        env = newEnv;
        visit(expr.body);
        env = newEnv.up;
    }

    void visit(in ExprWith expr) {
        scope(failure) writeln("while evaluating the with expression ", expr);
        auto newEnv = new Env(env, null, expr.attrs);
        env = newEnv;
        visit(expr.body);
        env = newEnv.up;
    }

    void visit(in ExprIf expr) {
        scope(failure) writeln("while evaluating the expression ", expr);
        if (visit(expr.cond).boolean)
            visit(expr.then);
        else
            visit(expr.else_);
    }

    void visit(in ExprAssert expr) {
        scope(failure) writeln("while evaluating the expression ", expr);
        import nix.printer : format;
        enforce!AssertionException(visit(expr.cond).boolean, format(expr.cond).idup);
        visit(expr.body);
    }
}

public ref Value eval(in Expr expr, ref Env env = createBaseEnv()) /*pure*/ {
    assert(expr);
    debug if (auto v = expr in env.cache) {
        assert(0);
        //return *v;
    }
    debug(EVAL) writeln("eval ", expr, " in env ", env.vars);
    auto ev = new Evaluator(&env);
    expr.accept(ev);
    // env.cache[expr] = ev.value;
    // assert(ev.value.type != Type.Thunk);
    return *ev.value;//env.cache[expr];
}

unittest {
    auto ZERO = new ExprInt(LOC, 0);
    auto false_ = new ExprVar(LOC, "false");
    auto true_ = new ExprVar(LOC, "true");
    auto ok = Value("ok", null);

    assert(eval(new ExprBinaryOp(LOC, Tok.SUB, ZERO, new ExprFloat(LOC, 3))) == Value(-3.0));
    assert(eval(new ExprBinaryOp(LOC, Tok.AND, false_, true_)) == Value(false));
    assert(eval(new ExprBinaryOp(LOC, Tok.OR, false_, true_)) == Value(true));

    auto att = new ExprAttrs(LOC, );
    // { a = "ok"; }
    att.attrs["a"] = ExprAttrs.AttrDef(new ExprString(LOC, "ok"));
    assert(eval(att) == Value(["a": Attr(&ok)]));
    // { a = "ok"; true = true; }
    att.attrs["true"] = ExprAttrs.AttrDef(true_, true);
    assert(eval(att) == Value(["a": Attr(&ok), "true": Attr(new Value(true))]));
    // { a = "ok"; true = true; b = "p"; }
    att.dynamicAttrs ~= ExprAttrs.DynamicAttrDef(new ExprString(LOC, "p"), new ExprString(LOC, "b"));
    assert(eval(att) == Value(["a": Attr(&ok), "true": Attr(new Value(true)), "b": Attr(new Value("p", null))]));
    // rec { a = "ok"; true = true; b = "p"; }
    att.recursive = true;
    // rec { a = "ok"; true = true; b = "p"; c = a; }
    att.attrs["c"] = ExprAttrs.AttrDef(new ExprVar(LOC, "a"));
    assert(eval(att).forceValueDeep == Value(["a": Attr(&ok), "true": Attr(new Value(true)), "b": Attr(new Value("p", null)), "c": Attr(&ok)])); //needs `rec`
    // rec { a = "ok"; true = true; b = "p"; c = a; d = b; }
    att.dynamicAttrs ~= ExprAttrs.DynamicAttrDef(new ExprVar(LOC, "b"), new ExprString(LOC, "d"));
    assert(eval(att).forceValueDeep == Value(["a": Attr(&ok), "true": Attr(new Value(true)), "b": Attr(new Value("p", null)), "c": Attr(&ok), "d": Attr(new Value("p", null))])); //needs `rec`
    assert(eval(new ExprFloat(LOC, 1.1)) == Value(1.1));
    assert(eval(new ExprInt(LOC, 42)) == Value(42));
    assert(eval(new ExprString(LOC, "foo")) == Value("foo", null));
    assert(eval(new ExprPath(LOC, "/fo/o")) == Value("/fo/o"));
    assert(eval(false_) == Value(false));
    assert(eval(new ExprVar(LOC, "null")) == Value());
    assert(eval(true_) == Value(true));
    assert(eval(new ExprAssert(LOC, true_, new ExprString(LOC, "ok"))) == ok);
    assert(eval(new ExprOpNot(LOC, true_)) == Value(false));
    assert(eval(new ExprOpNot(LOC, false_)) == Value(true));
    assert(eval(new ExprIf(LOC, true_, new ExprString(LOC, "ok"), new ExprFloat(LOC, 1.1))) == ok);
    assert(eval(new ExprIf(LOC, false_, new ExprFloat(LOC, 1.1), new ExprString(LOC, "ok"))) == ok);
    assert(eval(new ExprList(LOC, [new ExprString(LOC, "ok")])) == Value([&ok]));
}
