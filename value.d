module nix.value;

public import nix.parser;

debug import std.stdio : writeln;
import std.conv : to;

alias Bindings = Value[string];

alias PrimOp = Value function(in Value[] args...) /*pure*/;

struct Env {
    const(Env)* up;
    const(Bindings) vars;
    // const(ExprWith) hasWith;
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

string typeOf(in Value v) pure nothrow {
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
    // static auto EMPTY = Value(cast(Value[])[]);
    // static const ZERO = Value(0);
    static immutable FALSE = Value(false);
    static immutable TRUE = Value(true);

    Type type;
    private union {
        struct {
            string s;
            // string[] context; TODO
        }

        // string path;
        NixInt i;
        NixFloat f;
        bool b;
        const(Value)[] l;
        const(Bindings) a;
        const(ExprLambda) el;
        struct {
            const(Expr) t;
            const(Env)* e;
        }
        PrimOp p;
    }

    this(string str, in string[] context) pure {
        this.type = Type.String;
        this.s = str;
    }

    this(string path) pure {
        this.type = Type.Path;
        this.s = path;
    }

    this(NixInt integer) pure {
        this.type = Type.Int;
        this.i = integer;
    }

    this(NixFloat fpoint) pure {
        this.type = Type.Float;
        this.f = fpoint;
    }

    this(bool boolean) pure {
        this.type = Type.Bool;
        this.b = boolean;
    }

    this(in Value[] list) pure {
        this.type = Type.List;
        this.l = list;
    }

    this(in Bindings attrs) pure {
        this.type = Type.Attrs;
        this.a = attrs;
    }

    this(in ExprLambda lambda, in ref Env env) pure {
        this.type = Type.Lambda;
        this.el = lambda;
        this.e = &env;
    }

    this(in Expr val, in ref Env env) pure {
        this.type = Type.Thunk;
        this.t = val;
        this.e = &env;
    }

    this(PrimOp primOp) pure {
        this.type = Type.PrimOp;
        this.p = primOp;
    }

    this(Value left, Value right) pure {
        this.type = Type.App;
        this.l = [left, right];
    }

    @property bool isNull() pure const nothrow {
        return type == Type.Null;
    }

    @property string str() pure const nothrow {
        assert(type == Type.String, "value is "~typeOf(this)~" while a string was expected");
        return s;
    }

    @property NixInt integer() pure const nothrow {
        assert(type == Type.Int, "value is "~typeOf(this)~" while an integer was expected");
        return i;
    }

    @property NixFloat fpoint() pure const nothrow {
        assert(type == Type.Float, "value is "~typeOf(this)~" while a float was expected");
        return f;
    }

    @property string path() pure const nothrow {
        assert(type == Type.Path, "value is "~typeOf(this)~" while a path was expected");
        return s;
    }

    @property const(Bindings) attrs() pure const nothrow {
        assert(type == Type.Attrs, "value is "~typeOf(this)~" while a set was expected");
        return a;
    }

    @property bool boolean() pure const nothrow {
        assert(type == Type.Bool, "value is "~typeOf(this)~" while a boolean was expected");
        return b;
    }

    @property const(Value)[] list() pure const nothrow {
        assert(type == Type.List, "value is "~typeOf(this)~" while a list was expected");
        return l;
    }

    @property const(Value)[2] app() pure const nothrow {
        assert(type == Type.App, "value is "~typeOf(this)~" while a function application was expected");
        return l[0..2];
    }

    @property const(Expr) thunk() pure const nothrow {
        assert(type == Type.Thunk, "value is "~typeOf(this)~" while a thunk was expected");
        return t;
    }

    @property real number() pure const nothrow {
        return type == Type.Int ? integer : (type == Type.Float ? fpoint : real.nan);
    }

    @property const(Env)* env() pure const nothrow {
        assert(type == Type.Lambda || type == Type.Thunk);
        return e;
    }

    @property const(ExprLambda) lambda() pure const nothrow {
        assert(type == Type.Lambda, "value is "~typeOf(this)~" while a lambda was expected");
        return el;
    }

    Value opBinary(string OP)(auto ref const Value rhs) pure const {
        switch (this.type) {
        case Type.Float:
            switch(rhs.type) {
            case Type.Float:
                return Value(mixin("this.f" ~ OP ~ "rhs.f"));
            case Type.Int:
                return Value(mixin("this.f" ~ OP ~ "rhs.i"));
            default: assert(0, "cannot add to float");
            }
        case Type.Int:
            switch(rhs.type) {
            case Type.Float:
                return Value(mixin("this.i" ~ OP ~ "rhs.f"));
            case Type.Int:
                return Value(mixin("this.i" ~ OP ~ "rhs.i"));
            default: assert(0, "cannot add to integer");
            }
        case Type.Path:
            // TODO: support path + path
        case Type.String:
            static if (OP == "+") {
            assert(rhs.type == Type.String, "cannot coerce to string");
            return Value(this.s ~ rhs.s, null);
            }
        default: assert(0, "No operator "~OP~" for type "~typeOf(this));
        }
    }

    private static int cmp(L,R)(ref const L lhs, ref const R rhs) pure @safe {
        // static if (__traits(compiles, L.init.opCmp(R.init)))
        return (lhs > rhs) - (lhs < rhs);
    }

    int opCmp()(auto ref const Value rhs) pure const {
        switch (this.type) {
        case Type.Float:
            switch(rhs.type) {
            case Type.Float:
                return cmp(this.f, rhs.f);
            case Type.Int:
                return cmp(this.f, rhs.i);
            default: assert(0, "cannot compare to float");
            }
        case Type.Int:
            switch(rhs.type) {
            case Type.Float:
                return cmp(this.i, rhs.f);
            case Type.Int:
                return cmp(this.i, rhs.i);
            default: assert(0, "cannot compare to integer");
            }
        case Type.Path:
        case Type.String:
            return cmp(this.s, rhs.s);
        default: assert(0, "Cannot compare type "~typeOf(this));
        }
    }

    bool opEquals()(auto ref const Value v) pure const {
        final switch (type) {
        case Type.Null:
            return type == v.type;
        case Type.Path:
        case Type.String:
            return type == v.type && s == v.s;
        case Type.Int:
            return i == v.number;
        case Type.Float:
            return f == v.number;
        case Type.Bool:
            return type == v.type && b == v.b;
        case Type.App:
        case Type.PrimOpApp:
        case Type.List:
            return type == v.type && l == v.l;
        case Type.Attrs:
            return type == v.type && a == v.a;
        case Type.Lambda:
            return type == v.type && el is v.el && env == v.env;
        case Type.Thunk:
            return type == v.type && t is v.t && env == v.env;
        case Type.PrimOp:
            return type == v.type && p is v.p && env == v.env;
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
            return to!string(i);
        case Type.Float:
        version (PURE) {
            import std.format : printFloat, singleSpec;
            import std.string : strip;
            static immutable fs = singleSpec("%e");
            char[32] buf;
            return printFloat(buf[], f, fs).strip("e+0").idup;
        } else {
            return to!string(f);
        }
        case Type.Bool:
            return b ? "true" : "false";
        case Type.List:
            auto s = "[ ";
            foreach (e; l) s ~= e.toString() ~ ' ';
            return s ~ ']';
        case Type.Attrs:
            auto s = "{ ";
            foreach (k, v; a) s ~= k ~ " = " ~ v.toString() ~ "; ";
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

pure unittest {
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
}

unittest {
    assert(Value().toString() == "null");
    assert(Value(2).toString() == "2");
    assert(Value(2.5).toString() == "2.5");
    assert(Value("hello\n").toString() == `"hello\n"`);
    assert(Value(true).toString() == "true");
    assert(Value(false).toString() == "false");
    assert(Value([Value()]).toString() == "[ null ]");
    assert(Value(["n":Value()]).toString() == "{ n = null; }");
}

pure unittest {
    assert(Value(3) * Value(2) == Value(6));
    assert(Value(5) / Value(2) == Value(2));
    assert(Value(3) / Value(2.0) == Value(1.5));
    assert(Value(1.0) - Value(2) == Value(-1.0));
    assert(Value(1.0) + Value(2.0) == Value(3.0));
    assert(Value("a", null) + Value("b", null) == Value("ab", null));
    assert(Value("/a") + Value("/b") == Value("/a/b"));
    assert(Value("/a") + Value("str", null) == Value("/astr"));
    // assert(Value("a", null) + Value("/b") == Value("a/nix/store/asdf-b", null)); TODO

    assert(Value(3) < Value(12));
    assert(Value(1.0) < Value(2));
    assert(Value(2) <= Value(2.0));
    assert(Value(1.0) <= Value(2.0));
    assert(Value("/a") < Value("/b"));
    assert(Value("/a", null) < Value("/b", null));
}
