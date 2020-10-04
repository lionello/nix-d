module nix.evaluator;

import nix.parser;

import std.stdio : writeln, write;
import std.conv : to;
import std.format : printFloat, singleSpec;

alias Bindings = Value[string];
alias PrimOp = Value function(in Value[] args...) pure;

private struct Env {
    const(Env)* up;
    const(Bindings) vars;
    // ExprWith hasWith;
}

private Value prim_binOp(string OP)(in Value[] args...) pure if (__traits(compiles,"1"~OP~"2.2")) {
    assert(args.length == 2);
    switch (args[0].type) {
    case Type.Float:
        switch(args[1].type) {
        case Type.Float:
            return Value(mixin("args[0].fpoint" ~ OP ~ "args[1].fpoint"));
        case Type.Int:
            return Value(mixin("args[0].fpoint" ~ OP ~ "args[1].integer"));
        default: assert(0, "cannot add to float");
        }
    case Type.Int:
        switch(args[1].type) {
        case Type.Float:
            return Value(mixin("args[0].integer" ~ OP ~ "args[1].fpoint"));
        case Type.Int:
            return Value(mixin("args[0].integer" ~ OP ~ "args[1].integer"));
        default: assert(0, "cannot add to integer");
        }
    case Type.Path:
        // TODO: support path + path
    case Type.String:
        static if (OP == "+") {
        assert(args[1].type == Type.String, "cannot coerce to string");
        return Value(args[0].s ~ args[1].s);
        }
    default: assert(0, "No operator "~OP~" for type "~prim_typeOf(args[0]).s);
    }
}

unittest {
    assert(prim_binOp!"*"(Value(3), Value(2)) == Value(6));
    assert(prim_binOp!"/"(Value(3), Value(2.0)) == Value(1.5));
    assert(prim_binOp!"-"(Value(1.0), Value(2)) == Value(-1.0));
    assert(prim_binOp!"+"(Value(1.0), Value(2.0)) == Value(3.0));
    assert(prim_binOp!"+"(Value("a"), Value("b")) == Value("ab"));
}

private Value prim_typeOf(in Value[] args...) pure {
    assert(args.length == 1);
    final switch(args[0].type) {
    case Type.Null: return Value("null");
    case Type.String: return Value("string");
    case Type.Int: return Value("int");
    case Type.Float: return Value("float");
    case Type.Bool: return Value("bool");
    case Type.List: return Value("list");
    case Type.Attrs: return Value("set");
    case Type.Path: return Value("path");
    case Type.App:
    case Type.Lambda:
    case Type.PrimOp:
    case Type.PrimOpApp: return Value("lambda");
    case Type.Thunk: assert(0, "TODO must force value");
    }
}

private Value prim_isNull(in Value[] args...) pure {
    assert(args.length == 1);
    return Value(args[0].type == Type.Null);
}

private Value prim_lessThan(in Value[] args...) pure {
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

private Value lessOrEqThan(in Value[] args...) pure {
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

private Value prim_toString(in Value[] args...) pure {
    assert(args.length == 1);
    return Value(args[0].toString());
}

private const Env staticBaseEnv;

static this() {
    import core.stdc.time : time;

    static Value notImplemented(in Value[] args...) pure {
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
        "__elem" : Value(&notImplemented), "__elemAt" : Value(&notImplemented),
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
        "__hashString" : Value(&notImplemented), "__head" : Value(&notImplemented),
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

    void visit(Expr e, const ref Env env) {
        if (e) {
            const prevEnv = this.env;
            this.env = &env;
            e.accept(this);
            this.env = prevEnv;
        }
    }

    void visit(ExprOpNot e) {
        visit(e.expr, *env);
    }

    void visit(ExprBinaryOp e) {
        visit(e.left, *env);
        visit(e.right, *env);
    }

    void visit(ExprInt) {
    }

    void visit(ExprFloat) {
    }

    void visit(ExprString) {
    }

    void visit(ExprPath) {
    }

    void visit(ExprVar e) {
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

    void visit(ExprSelect e) {
        visit(e.e, *env);
        visit(e.def, *env);
        foreach (a; e.ap)
            visit(a.expr, *env);
    }

    void visit(ExprOpHasAttr e) {
        visit(e.expr, *env);
        foreach (a; e.ap)
            visit(a.expr, *env);
    }

    void visit(ExprAttrs e) {
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

    void visit(ExprList e) {
        foreach (i; e.elems)
            visit(i, *env);
    }

    void visit(ExprLambda e) {
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

    void visit(ExprLet e) {
        auto newEnv = Env(env);
        foreach (k, v; e.attrs.attrs) {
            newEnv.vars[k] = Value();
        }
        foreach (k, i; e.attrs.attrs) {
            visit(i.value, i.inherited ? *env : newEnv);
        }
        visit(e.body, newEnv);
    }

    void visit(ExprWith e) {
        visit(e.attrs, *env);
        auto newEnv = Env(env, true);
        visit(e.body, newEnv);
    }

    void visit(ExprIf e) {
        visit(e.cond, *env);
        visit(e.then, *env);
        visit(e.else_, *env);
    }

    void visit(ExprAssert e) {
        visit(e.cond, *env);
        visit(e.body, *env);
    }
}
*/

enum Type : byte {
    // Blackhole,
    Null,
    String,
    Int,
    Float,
    Bool,
    List,
    // List1,
    // List2,
    Attrs,
    Path,
    Lambda,
    Thunk,
    PrimOp,
    PrimOpApp,
    App,
    // Error,
}

private string escapeString(string s) @safe pure nothrow {
    char[] buf;
    buf.reserve(s.length + 2);
    buf ~= '"';
    foreach (c; s) {
        switch (c) {
        case '\\': buf ~= `\\`; break;
        case '$': buf ~= `\$`; break;
        case '"': buf ~= `\"`; break;
        case '\t': buf ~= `\t`; break;
        case '\n': buf ~= `\n`; break;
        case '\r': buf ~= `\r`; break;
        default: buf ~= c; break;
        }
    }
    buf ~= '"';
    return buf;
}

unittest {
    assert(escapeString("\\$\"\n\t\r") == `"\\\$\"\n\t\r"`);
}

struct Value {
    static auto EMPTY = Value(cast(Value[])[]);
    static auto ZERO = Value(0);
    static auto false_ = Value(false);
    static auto true_ = Value(true);

    Type type;
    union {
        struct {
            string s;
            // string[] context; TODO
        }

        // string path;
        NixInt integer;
        NixFloat fpoint;
        bool boolean;
        const(Value)[] list;
        const(Bindings) attrs;
        struct {
            ExprLambda lambda;
            const(Env)* lambdaEnv;
        }

        PrimOp primOp;
        struct {
            Expr thunk;
            const(Env)* env;
        }
    }

    this(string s) pure {
        this.type = Type.String;
        this.s = s;
    }

    this(NixInt integer) pure {
        this.type = Type.Int;
        this.integer = integer;
    }

    this(NixFloat fpoint) pure {
        this.type = Type.Float;
        this.fpoint = fpoint;
    }

    this(bool boolean) pure {
        this.type = Type.Bool;
        this.boolean = boolean;
    }

    this(in Value[] list) pure {
        this.type = Type.List;
        this.list = list;
    }

    this(in Bindings attrs) pure {
        this.type = Type.Attrs;
        this.attrs = attrs;
    }

    this(ExprLambda lambda, in ref Env env) pure {
        this.type = Type.Lambda;
        this.lambda = lambda;
        this.env = &env;
    }

    this(Expr val, in ref Env env) pure {
        this.type = Type.Thunk;
        this.thunk = val;
        this.env = &env;
    }

    this(PrimOp primOp) pure {
        this.type = Type.PrimOp;
        this.primOp = primOp;
    }

    this(Value left, Value right) pure {
        this.type = Type.PrimOpApp;
        this.list = [left, right];
    }

    @property real number() pure const nothrow {
        return type == Type.Int ? integer : (type == Type.Float ? fpoint : real.nan);
    }

    Value opBinary(string OP)(auto ref const Value rhs) pure const {
        return prim_binOp!OP(this, rhs);
    }

    bool opEquals()(auto ref const Value v) const {
        final switch (type) {
        case Type.Null:
            return type == v.type;
        case Type.Path:
        case Type.String:
            return type == v.type && s == v.s;
        case Type.Int:
            return integer == v.number;
        case Type.Float:
            return fpoint == v.number;
        case Type.Bool:
            return type == v.type && boolean == v.boolean;
        case Type.App:
        case Type.PrimOpApp:
        case Type.List:
            return type == v.type && list == v.list;
        case Type.Attrs:
            return type == v.type && attrs == v.attrs;
        case Type.Lambda:
            return type == v.type && lambda == v.lambda && env == v.env;
        case Type.Thunk:
            return type == v.type && thunk == v.thunk && env == v.env;
        case Type.PrimOp:
            return type == v.type && primOp == v.primOp && env == v.env;
        }
    }

    string toString() const pure {
        final switch (type) {
        case Type.Null:
            return "null";
        case Type.Path:
        case Type.String:
            return escapeString(s);
        case Type.Int:
            return to!string(integer);
        case Type.Float:
            char[32] buf;
            import std.string : strip;
            static immutable fs = singleSpec("%e");
            return printFloat(buf[], fpoint, fs).strip("e+0").idup; //to!string(fpoint);
        case Type.Bool:
            return boolean ? "true" : "false";
        case Type.List:
            auto s = "[ ";
            foreach (e; list) s ~= e.toString() ~ ' ';
            return s ~ ']';
        case Type.Attrs:
            auto s = "{ ";
            foreach (k, v; attrs) s ~= k ~ " = " ~ v.toString() ~ "; ";
            return s ~ '}';
        case Type.Lambda:
            return "<LAMBDA>";
        case Type.App:
        case Type.Thunk:
            return "<CODE>";
        case Type.PrimOp:
            return "<PRIMOP>";
        case Type.PrimOpApp:
            return "<PRIMOP-APP>";
        }
    }
}

unittest {
    assert(Value() == Value());
    assert(Value() != Value(1));
    assert(Value(2) == Value(2.0));
    assert(Value(2.5) != Value(2));
    assert(Value("a") == Value("ba"[1..$]));
    assert(Value("a") != Value());
    assert(Value(true) == Value(true));
    assert(Value(false) == Value(false));
    assert(Value(false) != Value(true));
    assert(Value([Value(3),Value()]) == Value([Value(3.0),Value()]));
    assert(Value(["n":Value(4)]) == Value(["n":Value(4.0)]));
    assert(Value(["n":Value()]) != Value(["k":Value()]));

    assert(Value().toString() == "null");
    assert(Value(2).toString() == "2");
    assert(Value(2.5).toString() == "2.5");
    assert(Value("hello\n").toString() == `"hello\n"`);
    assert(Value(true).toString() == "true");
    assert(Value(false).toString() == "false");
    assert(Value([Value()]).toString() == "[ null ]");
    assert(Value(["n":Value()]).toString() == "{ n = null; }");
}

class Thunker : Visitor {
    Env env;
    Value value;
    void thunk(Expr e) {
        value = Value(e, env);
    }

    void visit(ExprOpNot e) {
        thunk(e);
    }

    void visit(ExprBinaryOp e) {
        thunk(e);
    }

    void visit(ExprInt) {
    }

    void visit(ExprFloat) {
    }

    void visit(ExprString) {
    }

    void visit(ExprPath) {
    }

    void visit(ExprVar e) {
        // env.vars[e.name]
    }

    void visit(ExprSelect e) {
        thunk(e);
    }

    void visit(ExprOpHasAttr e) {
        thunk(e);
    }

    void visit(ExprAttrs e) {
        thunk(e);
    }

    void visit(ExprList e) {
        thunk(e);
    }

    void visit(ExprLambda e) {
        thunk(e);
    }

    void visit(ExprLet e) {
        thunk(e);
    }

    void visit(ExprWith e) {
        thunk(e);
    }

    void visit(ExprIf e) {
        thunk(e);
    }

    void visit(ExprAssert e) {
        thunk(e);
    }
}

class Evaluator : Visitor {
    const(Env)* env = &staticBaseEnv;
//    Env env = Env(&staticBaseEnv);
    Value value;

    Value visit(Expr e) {
        if (e)
            e.accept(this);
        return value;
    }

    void visit(ExprOpNot e) {
        visit(e.expr);
        assert(value.type == Type.Bool);
        value.boolean = !value.boolean;
    }

    void visit(ExprBinaryOp e) {
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

    void visit(ExprInt e) {
        value = Value(e.n);
    }

    void visit(ExprFloat e) {
        value = Value(e.f);
    }

    void visit(ExprString e) {
        value = Value(e.s);
    }

    void visit(ExprPath e) {
        value = Value(e.p);
    }

    void visit(ExprVar e) {
        value = lookupVar(e.name);
        // forceValue();
        // value = env.vars[e.name];
    }

    string getName(ref AttrName an) {
        if (an.ident) {
            return an.ident;
        } else {
            visit(an.expr);
            assert(value.type == Type.String);
            return value.s;
        }
    }

    void visit(ExprSelect e) {
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

    void visit(ExprOpHasAttr e) {
        visit(e.left);
        foreach (a; e.ap) {
            // forceValue?
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

    void visit(ExprAttrs e) {
        const(Env)* dynamicEnv = env;
        Bindings attrs;
        if (e.recursive) {
            assert(0, "not implemented: rec");
            // auto overrides = "__overrides" in e.attrs;

            // auto newEnv = Env(env, false, attrs);
            // foreach (k, v; e.attrs) {
            //     if (overrides && !v.inherited) {
            //         attrs[k] = mkThunk(v.value, newEnv);
            //     } else {
            //         attrs[k] = maybeThunk(v.value, v.inherited ? *env : newEnv);
            //     }
            // }

            // if (overrides) {
            //     assert(overrides.type == Type.Attrs);
            //     auto overrides = attrs[overrides.attrs;
            // }
            // dynamicEnv = &newEnv;
        } else {
            foreach (k, v; e.attrs) {
                attrs[k] = visit(v.value);
            }
        }
        foreach (v; e.dynamicAttrs) {
            visit(v.name);
            assert(value.type == Type.String);
            const k = value.s;
            assert(k !in attrs);
            attrs[k] = visit(v.value);
        }
        value = Value(attrs);
    }

    Value mkThunk(Expr e, in ref Env env) {
        return Value(e, env);
    }

    Value maybeThunk(Expr e, in ref Env env) {
        // TODO: don't thunk values or known vars; use Thunker
        return mkThunk(e, env);
    }

    void visit(ExprList e) {
        Value[] vars;
        foreach (v; e.elems)
            vars ~= visit(v);
        value = Value(vars);
    }

    void visit(ExprLambda e) {
        value = Value(e, *env);
    }

    void visit(ExprLet e) {
        Bindings b;
        auto newEnv = Env(env, b);
        foreach (k, v; e.attrs.attrs) {
            b[k] = maybeThunk(v.value, v.inherited ? *env : newEnv);
        }
        env = &newEnv;
        visit(e.body);
        env = newEnv.up;
    }

    void visit(ExprWith e) {
        debug writeln(e.attrs);
        auto att = visit(e.attrs);
        assert(att.type == Type.Attrs, to!string(att.type) ~ ", expected attrs");
        const newEnv = Env(env, att.attrs);// TODO: make lazy
        env = &newEnv;
        visit(e.body);
        env = newEnv.up;
    }

    void visit(ExprIf e) {
        if (visit(e.cond).boolean)
            visit(e.then);
        else
            visit(e.else_);
    }

    void visit(ExprAssert e) {
        assert(visit(e.cond).boolean);
        visit(e.body);
    }
}

unittest {
    auto eval(Expr e) {
        auto ev = new Evaluator;
        e.accept(ev);
        return ev.value;
    }

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
