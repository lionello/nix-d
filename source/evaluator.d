module nix.evaluator;

public import nix.value;
import nix.primops : createBaseEnv;
import nix.visitor : ConstVisitorT;
import nix.path;

debug import std.stdio : writeln;
import std.exception : enforce;

class EvalException : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) pure {
        super(msg, file, line);
    }
}

class AssertionException : EvalException {
    this(string msg, string file = __FILE__, size_t line = __LINE__) pure {
        super("assertion "~msg~" failed", file, line);
    }
}

// Helper for unittest
private @property Loc L(int line = __LINE__) { return Loc(line); }

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
        if (f.lambda.arg) {
            // add f.lambda.arg to the new env
            env2.vars[f.lambda.arg] = arg.dup;
        }
        if (f.lambda.formals) {
            auto attrs = forceValue(arg, pos).attrs;
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
                    env2.vars[formal.name] = new Value(formal.def, env2);
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

ref Value forceValue(return ref Value value, in Loc pos = Loc()) {
    debug writeln("forceValue ", value);
    switch (value.type) {
    // case Type.Ref:
    //     value = forceValue(value.deref);
    //     break;
    case Type.Thunk:
        auto expr = value.thunk;
        auto env = value.env;
        value = Value(double.nan); // avoid infinite recursion
        value = eval(expr, *env);
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
    assert(eval(new ExprVar(L, "__currentTime")).integer);
    assert(eval(new ExprVar(L, "__typeOf")).primOpArgs);
    assert("int" == eval(new ExprBinaryOp(L, Tok.APP, new ExprVar(L, "__typeOf"), new ExprInt(L, 2))).str);
    assert(eval(new ExprVar(L, "__add")).primOpArgs);
    assert(eval(new ExprBinaryOp(L, Tok.APP, new ExprVar(L, "__add"), new ExprInt(L, 2))).primOpArgs);
    assert(5 == eval(new ExprBinaryOp(L, Tok.APP, new ExprBinaryOp(L, Tok.APP, new ExprVar(L, "__add"), new ExprInt(L, 2)), new ExprInt(L, 3))).integer);
}

string forceStringNoCtx(ref Value str) {
    forceValue(str);
    enforce!EvalException(!str.context.length, "the string '"~str.str~"' is not allowed to refer to a store path");
    return str.str;
}

String tryAttrsToString(ref Value value, bool coerceMore, bool copyToStore) {
    auto toString = "__toString" in value.attrs;
    if (!toString) return String.init;
    auto str = callFunction(**toString, value, Loc());
    return coerceToString(str, coerceMore, copyToStore);
}

String coerceToString(ref Value value, bool coerceMore = false, bool copyToStore = true) {
    forceValue(value);
    switch (value.type) {
    case Type.String:
        return value.str;
    case Type.Path:
        auto path = canonPath(value.path);
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
    new ExprInt(L, 42).accept(thunker);
    assert(thunker.value.integer == 42);
    new ExprList(L, [new ExprInt(L, 42)]).accept(thunker);
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
            debug writeln("comparing ", k, ": ", p, " vs ", pp);
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
        scope(failure) writeln("while evaluating the expression ", expr, " on ", expr.loc);
        value = new Value(!visit(expr.expr).boolean);
    }

    void visit(in ExprBinaryOp expr) {
        scope(failure) writeln("while evaluating the expression ", expr, " on ", expr.loc);
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
            auto lhs = visit(expr.left);
            auto rhs = visit(expr.right);
            if (forceValue(lhs).isNumber) {
                // Strangely enough, + operator cannot currently be overridden
                // auto __add = lookupVar("__add"); // can be overridden
                value = (lhs + forceValue(rhs)).dup;
            } else {
                auto str = coerceToString(rhs, false, lhs.type == Type.String);
                value = (lhs + Value(str, str.context)).dup;
            }
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
            enforce!TypeException(value.type == Type.Bool, "a boolean was expected");
            break;
        case Tok.APP:
            auto lhs = visit(expr.left);
            // TODO: detect tail recursion
            value = callFunction(lhs, maybeThunk(expr.right, env), expr.loc).dup;
            break;
        default:
            assert(0);
        }
    }

    ref Value lookupVar(string name) {
        /* Check whether the variable appears in the environment. */
        for (auto curEnv = this.env; curEnv; curEnv = curEnv.up) {
            debug writeln("lookupVar ", name, " in env ", curEnv.vars);
            if (auto v = name in curEnv.vars) {
                // fromWith = false;
                return **v;
            }
        }
        /* Otherwise, the variable must be obtained from the nearest
        enclosing `with'.  If there is no `with', then we can issue an
        "undefined variable" error now. */
        for (auto curEnv = this.env; curEnv; curEnv = curEnv.up) {
            debug writeln("lookupVar(with) ", name, curEnv.withVars);
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
        scope(failure) writeln("while evaluating the expression ", expr, " on ", expr.loc);
        value = new Value(expr.n);
    }

    void visit(in ExprFloat expr) {
        scope(failure) writeln("while evaluating the expression ", expr, " on ", expr.loc);
        value = new Value(expr.f);
    }

    void visit(in ExprString expr) {
        scope(failure) writeln("while evaluating the expression ", expr, " on ", expr.loc);
        value = new Value(expr.s, null);
    }

    void visit(in ExprPath expr) {
        scope(failure) writeln("while evaluating the expression ", expr, " on ", expr.loc);
        value = new Value(absPath(expr.p));
    }

    void visit(in ExprVar expr) {
        scope(failure) writeln("while evaluating the expression ", expr, " on ", expr.loc);
        value = &forceValue(lookupVar(expr.name), expr.loc);
    }

    private string getName(in ref AttrName an) {
        if (an.ident) {
            return an.ident;
        } else {
            return forceStringNoCtx(eval(an.expr, *env));
        }
    }

    void visit(in ExprSelect expr) {
        scope(failure) writeln("while evaluating the expression ", expr, " on ", expr.loc);
        visit(expr.left);
        debug writeln("after expr.left=",*value, " for ", expr.ap);
        foreach (a; expr.ap) {
            scope(failure) writeln("while evaluating the attribute ", a, " on ", expr.loc);
            const name = getName(a);
            forceValue(*value, expr.loc); // TODO: use pos from attr 'a'
            debug writeln("look for ", name, " in ", *value);
            if (value.type == Type.Attrs) {
                if (auto j = name in value.attrs) {
                    debug writeln(" found ", name, ": ", **j, (*j).type == Type.Thunk ? (*j).env.vars : null);
                    value = *j;
                    continue;
                }
            }
            if (expr.def) {
                debug writeln("defaulting to ", expr.def);
                visit(expr.def);
                break;
            }
            import nix.printer : format;
            debug writeln(format(expr), expr.ap, "=", *value);
            throw new EvalException("attribute '"~name~"' missing");
        }
        scope(failure) writeln("while evaluating the attribute path ", expr.ap.toString(), " on ", expr.loc);
        forceValue(*value, expr.loc);
    }

    void visit(in ExprOpHasAttr expr) {
        scope(failure) writeln("while evaluating the expression ", expr, " on ", expr.loc);
        visit(expr.left);
        foreach (a; expr.ap) {
            scope(failure) writeln("while evaluating the attribute ", a);
            forceValue(*value, expr.loc);
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
        scope(failure) writeln("while evaluating the expression ", expr, " on ", expr.loc);
        auto dynamicEnv = env;
        Bindings attrs = empty;
        if (expr.recursive) {
            debug writeln("ExprAttrs (expr.recursive)");
            const hasOverrides = "__overrides" in expr.attrs;

            // Create a new environment that contains the attributes in this `rec'.
            auto newEnv = new Env(env, attrs);

            // The recursive attributes are evaluated in the new
            // environment, while the inherited attributes are evaluated
            // in the original environment.
            foreach (k, v; expr.attrs) {
                debug writeln("rec ", k, " in env ", (v.inherited ? env : newEnv).vars);
                if (hasOverrides && !v.inherited) {
                    attrs[k] = new Value(v.value, newEnv);
                    assert(k in newEnv.vars);
                } else {
                    attrs[k] = maybeThunk(v.value, v.inherited ? env : newEnv).dup;
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
                auto vOverrides = forceValue(*attrs["__overrides"], expr.loc).attrs;
                foreach(k, ref v; vOverrides) {
                    // auto j = k in expr.attrs;
                    attrs[k] = v; // overwrites
                }
            }
            dynamicEnv = newEnv;
            assert(dynamicEnv.vars is attrs);
            debug writeln("dynamicEnv is ", dynamicEnv.vars);
        } else {
            foreach (k, v; expr.attrs) {
                // attrs[k] = new Value(v.value, env);
                attrs[k] = maybeThunk(v.value, env).dup;
            }
        }
        // Dynamic attrs apply *after* rec and __overrides.
        foreach (attr; expr.dynamicAttrs) {
            debug writeln("dynamicAttr ", attr.name);
            env = dynamicEnv;
            auto nameVal = visit(attr.name);
            debug writeln("dynamicAttr ", nameVal);
            env = dynamicEnv.up;
            forceValue(nameVal, expr.loc);
            if (nameVal.type == Type.Null) {
                continue;
            }
            const nameSym = forceStringNoCtx(nameVal);
            enforce!EvalException(nameSym !in attrs, "dynamic attribute already defined: "~nameSym);
            attrs[nameSym] = maybeThunk(attr.value, dynamicEnv).dup;
        }
        debug foreach (k, v; attrs) {
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
        scope(failure) writeln("while evaluating the expression ", expr, " on ", expr.loc);
        // Create a new environment that contains the attributes in this `let'.
        auto newEnv = new Env(env);
        // The recursive attributes are evaluated in the new environment,
        // while the inherited attributes are evaluated in the original
        // environment.
        foreach (k, attr; expr.attrs.attrs) {
            newEnv.vars[k] = maybeThunk(attr.value, attr.inherited ? env : newEnv).dup;
        }
        env = newEnv;
        visit(expr.body);
        env = newEnv.up;
    }

    void visit(in ExprWith expr) {
        scope(failure) writeln("while evaluating the expression ", expr, " on ", expr.loc);
        auto newEnv = new Env(env, null, expr.attrs);
        env = newEnv;
        visit(expr.body);
        env = newEnv.up;
    }

    void visit(in ExprIf expr) {
        scope(failure) writeln("while evaluating the expression ", expr, " on ", expr.loc);
        if (visit(expr.cond).boolean)
            visit(expr.then);
        else
            visit(expr.else_);
    }

    void visit(in ExprAssert expr) {
        scope(failure) writeln("while evaluating the expression ", expr, " on ", expr.loc);
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
    debug writeln("eval ", expr, " in env ", env.vars);
    auto ev = new Evaluator(&env);
    expr.accept(ev);
    // env.cache[expr] = ev.value;
    // assert(ev.value.type != Type.Thunk);
    return *ev.value;//env.cache[expr];
}

unittest {
    auto ZERO = new ExprInt(L, 0);
    auto false_ = new ExprVar(L, "false");
    auto true_ = new ExprVar(L, "true");
    auto ok = Value("ok", null);

    assert(eval(new ExprBinaryOp(L, Tok.SUB, ZERO, new ExprFloat(L, 3))) == Value(-3.0));
    assert(eval(new ExprBinaryOp(L, Tok.AND, false_, true_)) == Value(false));
    assert(eval(new ExprBinaryOp(L, Tok.OR, false_, true_)) == Value(true));

    auto att = new ExprAttrs(L, );
    // { a = "ok"; }
    att.attrs["a"] = ExprAttrs.AttrDef(new ExprString(L, "ok"));
    assert(eval(att) == Value(["a": &ok]));
    // { a = "ok"; true = true; }
    att.attrs["true"] = ExprAttrs.AttrDef(true_, true);
    assert(eval(att) == Value(["a": &ok, "true": new Value(true)]));
    // { a = "ok"; true = true; b = "p"; }
    att.dynamicAttrs ~= ExprAttrs.DynamicAttrDef(new ExprString(L, "p"), new ExprString(L, "b"));
    assert(eval(att) == Value(["a": &ok, "true": new Value(true), "b": new Value("p", null)]));
    // rec { a = "ok"; true = true; b = "p"; }
    att.recursive = true;
    // rec { a = "ok"; true = true; b = "p"; c = a; }
    att.attrs["c"] = ExprAttrs.AttrDef(new ExprVar(L, "a"));
    assert(eval(att).forceValueDeep == Value(["a": &ok, "true": new Value(true), "b": new Value("p", null), "c": &ok])); //needs `rec`
    // rec { a = "ok"; true = true; b = "p"; c = a; d = b; }
    att.dynamicAttrs ~= ExprAttrs.DynamicAttrDef(new ExprVar(L, "b"), new ExprString(L, "d"));
    assert(eval(att).forceValueDeep == Value(["a": &ok, "true": new Value(true), "b": new Value("p", null), "c": &ok, "d": new Value("p", null)])); //needs `rec`
    assert(eval(new ExprFloat(L, 1.1)) == Value(1.1));
    assert(eval(new ExprInt(L, 42)) == Value(42));
    assert(eval(new ExprString(L, "foo")) == Value("foo", null));
    assert(eval(new ExprPath(L, "/fo/o")) == Value("/fo/o"));
    assert(eval(false_) == Value(false));
    assert(eval(new ExprVar(L, "null")) == Value());
    assert(eval(true_) == Value(true));
    assert(eval(new ExprAssert(L, true_, new ExprString(L, "ok"))) == ok);
    assert(eval(new ExprOpNot(L, true_)) == Value(false));
    assert(eval(new ExprOpNot(L, false_)) == Value(true));
    assert(eval(new ExprIf(L, true_, new ExprString(L, "ok"), new ExprFloat(L, 1.1))) == ok);
    assert(eval(new ExprIf(L, false_, new ExprFloat(L, 1.1), new ExprString(L, "ok"))) == ok);
    assert(eval(new ExprList(L, [new ExprString(L, "ok")])) == Value([&ok]));
}
