module nix.value;

public import nix.parser;

import std.conv : to;

alias Bindings = Value[string];
alias PrimOp = Value function(in Value[] args...) /*pure*/;

struct Env {
    const(Env)* up;
    const(Bindings) vars;
    // ExprWith hasWith;
}

enum Type : byte {
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
    // External,
    // Error,
    // Blackhole,
}

string typeOf(Value v) pure {
    final switch(v.type) {
    case Type.Null: return "null";
    case Type.String: return "string";
    case Type.Int: return "int";
    case Type.Float: return "float";
    case Type.Bool: return "bool";
    case Type.List: return "list";
    case Type.Attrs: return "set";
    case Type.Path: return "path";
    case Type.App:
    case Type.Lambda:
    case Type.PrimOp:
    case Type.PrimOpApp: return "lambda";
    case Type.Thunk: assert(0, "TODO must force value");
    }
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
    static assert(escapeString("x\\$\"\n\t\r") == `"x\\\$\"\n\t\r"`);
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
            const(ExprLambda) lambda;
            const(Env)* lambdaEnv;
        }

        PrimOp primOp;
        struct {
            const(Expr) thunk;
            const(Env)* env;
        }
    }

    this(string s, string[] context) /*pure*/ {
        this.type = Type.String;
        this.s = s;
    }

    this(string s) /*pure*/ {
        this.type = Type.Path;
        this.s = s;
    }

    this(NixInt integer) /*pure*/ {
        this.type = Type.Int;
        this.integer = integer;
    }

    this(NixFloat fpoint) /*pure*/ {
        this.type = Type.Float;
        this.fpoint = fpoint;
    }

    this(bool boolean) /*pure*/ {
        this.type = Type.Bool;
        this.boolean = boolean;
    }

    this(in Value[] list) /*pure*/ {
        this.type = Type.List;
        this.list = list;
    }

    this(in Bindings attrs) /*pure*/ {
        this.type = Type.Attrs;
        this.attrs = attrs;
    }

    this(in ExprLambda lambda, in ref Env env) /*pure*/ {
        this.type = Type.Lambda;
        this.lambda = lambda;
        this.lambdaEnv = &env;
    }

    this(in Expr val, in ref Env env) /*pure*/ {
        this.type = Type.Thunk;
        this.thunk = val;
        this.env = &env;
    }

    this(PrimOp primOp) /*pure*/ {
        this.type = Type.PrimOp;
        this.primOp = primOp;
    }

    this(Value left, Value right) /*pure*/ {
        this.type = Type.PrimOpApp;
        this.list = [left, right];
    }

    @property real number() /*pure*/ const nothrow {
        return type == Type.Int ? integer : (type == Type.Float ? fpoint : real.nan);
    }

    Value opBinary(string OP)(auto ref const Value rhs) /*pure*/ const  if (__traits(compiles,"1"~OP~"2.2")) {
        switch (this.type) {
        case Type.Float:
            switch(rhs.type) {
            case Type.Float:
                return Value(mixin("this.fpoint" ~ OP ~ "rhs.fpoint"));
            case Type.Int:
                return Value(mixin("this.fpoint" ~ OP ~ "rhs.integer"));
            default: assert(0, "cannot add to float");
            }
        case Type.Int:
            switch(rhs.type) {
            case Type.Float:
                return Value(mixin("this.integer" ~ OP ~ "rhs.fpoint"));
            case Type.Int:
                return Value(mixin("this.integer" ~ OP ~ "rhs.integer"));
            default: assert(0, "cannot add to integer");
            }
        case Type.Path:
            // TODO: support path + path
        case Type.String:
            static if (OP == "+") {
            assert(rhs.type == Type.String, "cannot coerce to string");
            return Value(this.s ~ rhs.s);
            }
        default: assert(0, "No operator "~OP~" for type "~typeOf(this));
        }
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

    string toString() const /*pure*/ {
        final switch (type) {
        case Type.Null:
            return "null";
        case Type.Path:
        case Type.String:
            return escapeString(s);
        case Type.Int:
            return to!string(integer);
        case Type.Float:
        version (PURE) {
            import std.format : printFloat, singleSpec;
            import std.string : strip;
            static immutable fs = singleSpec("%e");
            char[32] buf;
            return printFloat(buf[], fpoint, fs).strip("e+0").idup;
        } else {
            return to!string(fpoint);
        }
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
    static assert(24 <= Value.sizeof);

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
