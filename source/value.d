module nix.value;

public import nix.parser;

debug import std.stdio : writeln;
import std.conv : to;

alias Bindings = Value[string];

@property
static Bindings empty() {
    Bindings _empty = ["": Value.NULL];
    _empty.remove("");
    return _empty;
}

unittest {
    assert(empty.length == 0);
    empty["x"] = Value.TRUE;
    assert(empty.length == 0);
    auto aa1 = empty();
    auto aa2 = aa1;
    aa2["y"] = Value.FALSE;
    assert(aa1 == aa2);
}

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

string typeOf(in Value v) @nogc @safe pure nothrow {
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

alias PathSet = bool[string];

struct Value {
    static immutable NULL = Value();
    static immutable FALSE = Value(false);
    static immutable TRUE = Value(true);

    private union {
        struct {
            string s;
            // PathSet c;
        }

        // string path;
        NixInt i;
        NixFloat f;
        bool b;
        const(Value)[] l;
        Bindings a;
        const(ExprLambda) el;
        struct {
            const(Expr) t;
            const(Env)* e;
        }
        PrimOp op;
    }
    Type type;

    this(string str, PathSet context) pure {
        this.type = Type.String;
        this.s = str;
        // this.c = context;
    }

    this(string path) pure {
        assert(path != "", "Path should not be empty");
        assert(path[0] == '/', "Path should be absolute: "~path);
        assert(path == "/" || path[$-1] != '/', "Path has trailing slash: "~path);
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

    this(Bindings attrs) pure {
        this.type = Type.Attrs;
        this.a = attrs;
    }

    this(in ExprLambda lambda, in Env* env) pure {
        assert(lambda);
        assert(env);
        this.type = Type.Lambda;
        this.el = lambda;
        this.e = env;
    }

    this(in Expr val, in Env* env) pure {
        assert(val);
        assert(env);
        this.type = Type.Thunk;
        this.t = val;
        this.e = env;
    }

    this(PrimOp primOp) pure {
        assert(primOp);
        this.type = Type.PrimOp;
        this.op = primOp;
    }

    this(PrimOp primOp, in Value[] args...) pure {
        assert(primOp);
        this.type = Type.PrimOpApp;
        this.l = Value(primOp) ~ args;
    }

    this(in Value left, in Value right) pure {
        this.type = Type.App;
        this.l = [left, right];
    }

    @property bool isNull() pure const nothrow {
        assert(type != Type.Thunk, "Must force value first");
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

    @property const(Value)[] app() pure const nothrow {
        assert(type == Type.App, "value is "~typeOf(this)~" while a function application was expected");
        assert(l.length == 2);
        return l;
    }

    @property const(Expr) thunk() pure const nothrow {
        assert(type == Type.Thunk, "value is "~typeOf(this)~" while a thunk was expected");
        return t;
    }

    @property real number() @nogc pure const nothrow {
        return type == Type.Int ? i : (type == Type.Float ? f : real.nan);
    }

    @property const(Env)* env() pure const nothrow {
        assert(type == Type.Lambda || type == Type.Thunk);
        return e;
    }

    @property const(ExprLambda) lambda() pure const nothrow {
        assert(type == Type.Lambda, "value is "~typeOf(this)~" while a lambda was expected");
        return el;
    }

    @property PrimOp primOp() pure const nothrow {
        if (type == Type.PrimOpApp) return l[0].primOp;
        assert(type == Type.PrimOp, "value is "~typeOf(this)~" while a function was expected");
        return op;
    }

    @property const(Value)[] primOpArgs() pure const nothrow {
        assert(type == Type.PrimOpApp, "value is "~typeOf(this)~" while a function application was expected");
        return l[1..$];
    }

    @property PathSet context() pure const nothrow {
        if (type == Type.Path) return [s:true];
        assert(type == Type.String, "value is "~typeOf(this)~" while a string was expected");
        return null;
    }

    Value opBinary(string OP)(auto ref const Value rhs) pure const {
        switch (this.type) {
        case Type.Float:
            static if (OP == "+" || OP == "-" || OP == "*" || OP == "/") {
            switch(rhs.type) {
            case Type.Int:
                return Value(mixin("this.f" ~ OP ~ "rhs.i"));
            case Type.Float:
                return Value(mixin("this.f" ~ OP ~ "rhs.f"));
            default:
            }
            }
            break;
        case Type.Int:
            switch(rhs.type) {
            case Type.Int:
                return Value(mixin("this.i" ~ OP ~ "rhs.i"));
            case Type.Float:
                static if (OP == "+" || OP == "-" || OP == "*" || OP == "/") {
                return Value(mixin("this.i" ~ OP ~ "rhs.f"));
                }
            default:
            }
            break;
        case Type.Path:
            static if (OP == "+") {
            assert(rhs.type == Type.String || rhs.type == Type.Path, "cannot coerce "~typeOf(rhs)~" to string");
            const lhs = this.s == "/." ? "/" : this.s;
            // FIXME: canonicalize
            import std.path : asNormalizedPath;
            return Value(asNormalizedPath(lhs ~ rhs.s).array);
            }
        case Type.String:
            static if (OP == "+") {
            assert(rhs.type == Type.String || rhs.type == Type.Path, "cannot coerce "~typeOf(rhs)~" to string");
            PathSet ps = this.context().dup;
            foreach (k, v; rhs.context()) ps[k] = v;
            return Value(this.s ~ rhs.s, ps);
            }
        default:
        }
        assert(0, "No operator "~OP~" for type "~typeOf(this)~" and "~typeOf(rhs));
    }

    private static int cmp(L,R)(ref const L lhs, ref const R rhs) @nogc pure @safe {
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
            default:
            }
            break;
        case Type.Int:
            switch(rhs.type) {
            case Type.Float:
                return cmp(this.i, rhs.f);
            case Type.Int:
                return cmp(this.i, rhs.i);
            default:
            }
            break;
        case Type.Path:
        case Type.String:
            return cmp(this.s, rhs.s);
        default:
        }
        assert(0, "cannot compare type "~typeOf(this)~" with "~typeOf(rhs));
    }

    bool opEquals()(auto ref const Value v) @trusted @nogc pure const nothrow {
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
        case Type.List:
            return type == v.type && l == v.l;
        case Type.Attrs:
            return type == v.type && a == v.a;
        case Type.PrimOp:
        case Type.PrimOpApp:
        case Type.Lambda:
            return false;
        case Type.Thunk:
            assert(0, "Should forceValue before calling opEquals");
        }
    }

    string toString(bool shallow = false) const /*pure*/ {
        final switch (type) {
        case Type.Null:
            return "null";
        case Type.Path:
            return s;
        case Type.String:
            import nix.printer : escapeString;
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
            if (shallow) return "[…]";
            auto s = "[ ";
            foreach (e; l) s ~= e.toString(true) ~ ' ';
            return s ~ ']';
        case Type.Attrs:
            if (shallow) return "{…}";
            auto s = "{ ";
            // FIXME: detect infinite recursion
            foreach (k, v; a) s ~= k ~ " = " ~ v.toString(true) ~ "; ";
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

    size_t toHash() pure const nothrow @trusted {
        final switch (type) {
        case Type.Null:
            return 0xDAEC0270;
        case Type.Path:
            return hashOf(s);
        case Type.String:
            return hashOf(s, 0x46FEB33A); // TODO context
        case Type.Int:
        case Type.Float:
            return hashOf(number);
        case Type.Bool:
            return b ? 0xCA1C5848 : 0x742D4705;
        case Type.List:
        case Type.App:
            return hashOf(l, type);
        case Type.Attrs:
            return hashOf(a);
        case Type.PrimOp:
        case Type.PrimOpApp:
        case Type.Lambda:
            assert(0, "Can't be hashed");
        case Type.Thunk:
            assert(0, "Should forceValue before calling toHash");
            // return t.toHash ^ env.vars.toHash;
        }
    }
}

unittest {
    static assert(24 <= Value.sizeof);

    assert(Value() == Value());
    assert(Value() != Value(1));
    assert(Value(2) == Value(2.0));
    assert(Value(2.5) != Value(2));
    assert(Value("/a") == Value("b/a"[1..$]));
    assert(Value("a", null) != Value());
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
    assert(Value("hello\n", null).toString() == `"hello\n"`);
    assert(Value(true).toString() == "true");
    assert(Value(false).toString() == "false");
    assert(Value([Value()]).toString() == "[ null ]");
    assert(Value(["n":Value()]).toString() == "{ n = null; }");
}

unittest {
    assert(Value(3) * Value(2) == Value(6));
    assert(Value(5) / Value(2) == Value(2));
    assert(Value(3) / Value(2.0) == Value(1.5));
    assert(Value(1.0) - Value(2) == Value(-1.0));
    assert(Value(1.0) + Value(2.0) == Value(3.0));
    assert(Value("a", null) + Value("b", null) == Value("ab", null));
    assert(Value("/a") + Value("/b") == Value("/a/b"));
    assert(Value("/a") + Value("str", null) == Value("/astr"));
    // assert(Value("a", null) + Value("/b") == Value("a/nix/store/asdf-b", null)); TODO
    assert(Value("/.") + Value("str/", null) == Value("/str"));

    assert(Value(3) < Value(12));
    assert(Value(1.0) < Value(2));
    assert(Value(2) <= Value(2.0));
    assert(Value(1.0) <= Value(2.0));
    assert(Value("/a") < Value("/b"));
    assert(Value("/a", null) < Value("/b", null));
}

unittest {
    const map = [
        Value(): true,
        Value("/"): true,
        Value("/string", null): true,
        Value(2): true,
        Value(3.0): true,
        Value(true): true,
        Value([Value()]): true,
        Value(["null":Value()]): true,
        Value(Value("/"), Value(2)): true,
    ];
    assert(Value() in map);
    assert(Value("/") in map);
    assert(Value("/", null) !in map);
    assert(Value("/string", null) in map);
    assert(Value("/string") !in map);
    assert(Value(2) in map);
    assert(Value(2.0) in map);
    assert(Value(3.0) in map);
    assert(Value(3) in map);
    assert(Value(4) !in map);
    assert(Value(true) in map);
    assert(Value(false) !in map);
    assert(Value([Value()]) in map);
    assert(Value([Value(1)]) !in map);
    assert(Value(["null":Value()]) in map);
    assert(Value(["null":Value("/")]) !in map);
    assert(Value(["asdf":Value()]) !in map);
    assert(Value(Value("/"), Value(2)) in map);
    assert(Value([Value("/"), Value(2)]) !in map);
    assert(Value(Value("/"), Value(2.0)) in map);
    assert(Value(Value("/"), Value(42)) !in map);
    assert(Value(Value("/", null), Value(2)) !in map);
}