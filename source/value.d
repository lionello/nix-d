module nix.value;

public import nix.parser;
import nix.path;

debug import std.stdio : writeln;
import std.conv : text;
import std.exception : enforce;

class TypeException : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) pure {
        super(msg, file, line);
    }
}

// alias LazyValue = Value delegate(); // TODO: use this instead of thunk

alias Attr = Value*;

// struct Attr {
//     Value *value;
//     Loc loc;
//     string toString() const @safe pure {
//         return "Attr("~value.toString()~", "~loc.toString()~")";
//     }
// }

alias Bindings = Attr[string];

@property
static Bindings empty() {
    Bindings _empty = ["": null];
    _empty.remove("");
    return _empty;
}

unittest {
    assert(empty.length == 0);
    empty["x"] = Value.TRUE.dup;
    assert(empty.length == 0);
    auto aa1 = empty();
    auto aa2 = aa1;
    aa2["y"] = Value.FALSE.dup;
    assert(aa1 == aa2);
}

alias PrimOp = Value function(Value*[] args...) /*pure*/;

struct Env {
    Env* up;
    Bindings vars;
    const(Expr) hasWith;
    Bindings withVars;
    debug Value[const(Expr)] cache;//temp
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
    // Ref,//test
    // External,
    // Error,
    // Blackhole,
}

string typeOf(in Value v) @nogc @trusted pure nothrow {
    final switch(v.type) {
    case Type.Null: return "null";
    case Type.String: return "string";
    case Type.Int: return "int";
    case Type.Float: return "float";
    case Type.Bool: return "bool";
    case Type.List: return "list";
    case Type.Attrs: return "set";
    case Type.Path: return "path";
    // case Type.Ref: return typeOf(*v.r);
    case Type.App:
    case Type.Lambda:
    case Type.PrimOp:
    case Type.PrimOpApp: return "lambda";
    case Type.Thunk: assert(0, "TODO must force value");
    }
}

alias PathSet = bool[Path];

public PathSet dupx(in PathSet ps) {
    PathSet result;
    foreach (k,v; ps) result[k] = v;
    return result;
}

struct String {
    alias raw this;
    string raw;
    const(PathSet) context;
}

unittest {
    assert(String.init is null);
}

private bool arrayPtrEquals(in Value*[] lhs, in Value*[] rhs) pure @nogc @safe {
    if (lhs.length != rhs.length) return false;
    foreach (i, p; lhs) if (*p != *rhs[i]) return false;
    return true;
}

unittest {
    assert(arrayPtrEquals([], []));
    assert(!arrayPtrEquals([], [new Value()]));
    assert(arrayPtrEquals([new Value()], [new Value()]));
    assert(arrayPtrEquals([new Value(2)], [new Value(2.0)]));
}

struct Value {
    @property
    static Value NULL() { return Value(); }
    @property
    static Value FALSE() { return Value(false); }
    @property
    static Value TRUE() { return Value(true); }

    private union {
        String s;
        Path p;
        NixInt i;
        NixFloat f;
        bool b;
        Value*[] l;
        // Value*[2] sl;
        Bindings a;
        const(ExprLambda) el;
        struct {
            const(Expr) t;
            Env* e;
        }
        PrimOp op;
    }
    Type type;
    // private bool list1, list2;

    // this(Value* r) pure {
    //     assert(r);
    //     this.type = Type.Ref;
    //     this.r = r;
    // }

    this(String str) pure {
        this(str.raw, str.context);
    }

    this(string str, in PathSet context) pure {
        // assert(str !is null);
        this.type = Type.String;
        this.s = String(str, context);
    }

    this(Path path) pure {
        assert(path != "", "Path should not be empty");
        assert(path[0] == '/', "Path should be absolute: "~path);
        assert(path == "/" || path[$-1] != '/', "Path has trailing slash: "~path);
        assert(path != "/.");
        this.type = Type.Path;
        this.p = path;
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

    this(Value*[] list) pure {
        this.type = Type.List;
        this.l = list;
    }

    this(Bindings attrs) pure {
        this.type = Type.Attrs;
        this.a = attrs;
    }

    this(in ExprLambda lambda, Env* env) pure {
        assert(lambda);
        assert(env);
        this.type = Type.Lambda;
        this.el = lambda;
        this.e = env;
    }

    this(in Expr expr, Env* env) pure {
        assert(expr);
        assert(env);
        this.type = Type.Thunk;
        this.t = expr;
        this.e = env;
    }

    this(PrimOp primOp) pure {
        assert(primOp);
        this.type = Type.PrimOp;
        this.op = primOp;
    }

    this(PrimOp primOp, Value*[] args...) pure {
        assert(primOp);
        this.type = Type.PrimOpApp;
        this.l = new Value(primOp) ~ args;
    }

    this(Value* left, Value* right) pure {
        assert(left.type == Type.PrimOpApp || left.type == Type.Attrs || left.type == Type.Lambda);
        assert(right);
        this.type = Type.App;
        this.l = [left, right];
    }

    private @property string _string() @nogc pure const nothrow {
        return type == Type.String ? s.raw : (type == Type.Path ? p : null);
    }

    @property real _number() @nogc pure const nothrow {
        return type == Type.Int ? i : (type == Type.Float ? f : real.nan);
    }

    @property bool isNull() pure const nothrow {
        assert(type != Type.Thunk, "Must force value first");
        return type == Type.Null;
    }

    @property bool isNumber() pure const nothrow {
        assert(type != Type.Thunk, "Must force value first");
        return type == Type.Float || type == Type.Int;
    }

    @property const(String) str() pure const {
        enforce!TypeException(type == Type.String, "value is "~typeOf(this)~" while a string was expected: "~toString());
        return s;
    }

    @property NixInt integer() pure const {
        enforce!TypeException(type == Type.Int, "value is "~typeOf(this)~" while an integer was expected: "~toString());
        return i;
    }

    @property NixFloat fpoint() pure const {
        enforce!TypeException(type == Type.Float, "value is "~typeOf(this)~" while a float was expected: "~toString());
        return f;
    }

    @property Path path() pure const {
        enforce!TypeException(type == Type.Path, "value is "~typeOf(this)~" while a path was expected: "~toString());
        return p;
    }

    @property Bindings attrs() pure {
        enforce!TypeException(type == Type.Attrs, "value is "~typeOf(this)~" while a set was expected: "~toString());
        return a;
    }

    @property const(Bindings) attrs() pure const {
        enforce!TypeException(type == Type.Attrs, "value is "~typeOf(this)~" while a set was expected: "~toString());
        return a;
    }

    @property bool boolean() pure const {
        enforce!TypeException(type == Type.Bool, "value is "~typeOf(this)~" while a boolean was expected: "~toString());
        return b;
    }

    @property Value*[] list() pure {
        enforce!TypeException(type == Type.List, "value is "~typeOf(this)~" while a list was expected: "~toString());
        return l;
    }

    @property Value*[] app() pure {
        enforce!TypeException(type == Type.App, "value is "~typeOf(this)~" while a function application was expected: "~toString());
        assert(l.length == 2);
        return l;
    }

    @property const(Expr) thunk() pure {
        enforce!TypeException(type == Type.Thunk, "value is "~typeOf(this)~" while a thunk was expected: "~toString());
        return t;
    }

    @property Env* env() pure nothrow {
        assert(type == Type.Lambda || type == Type.Thunk);
        return e;
    }

    @property const(ExprLambda) lambda() pure {
        enforce!TypeException(type == Type.Lambda, "value is "~typeOf(this)~" while a lambda was expected: "~toString());
        return el;
    }

    @property PrimOp primOp() pure const {
        if (type == Type.PrimOpApp) return l[0].primOp;
        enforce!TypeException(type == Type.PrimOp, "value is "~typeOf(this)~" while a function was expected: "~toString());
        return op;
    }

    @property Value*[] primOpArgs() pure {
        enforce!TypeException(type == Type.PrimOpApp, "value is "~typeOf(this)~" while a function application was expected: "~toString());
        return l[1..$];
    }

    @property const(PathSet) context() pure const {
        if (type == Type.Path) return [p:true];
        enforce!TypeException(type == Type.String, "value is "~typeOf(this)~" while a string was expected: "~toString());
        return s.context;
    }

    // @property ref Value deref() pure {
    //     return type == Type.Ref ? r.deref : this;
    // }

    @property Value* dup() {
        auto v = new Value;
        *v = this;
        return v;
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
            enforce!TypeException(rhs.type == Type.String || rhs.type == Type.Path, "cannot coerce "~typeOf(rhs)~" to string: "~rhs.toString());
            return Value(canonPath(this.s ~ rhs._string));
            }
        case Type.String:
            static if (OP == "+") {
            enforce!TypeException(rhs.type == Type.String || rhs.type == Type.Path, "cannot coerce "~typeOf(rhs)~" to string: "~rhs.toString());
            PathSet ps;
            foreach (k, v; this.context()) ps[k] = v;
            foreach (k, v; rhs.context()) ps[k] = v;
            return Value(this.s ~ rhs._string, ps);
            }
        default:
        }
        assert(0, "No operator "~OP~" for type "~typeOf(this)~" and "~typeOf(rhs));
    }

    private static int cmp(L,R)(in L lhs, in R rhs) @nogc pure @safe nothrow {
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
            return cmp(this.s, rhs._string);
        default:
        }
        assert(0, "cannot compare type "~typeOf(this)~" with "~typeOf(rhs));
    }

    bool opEquals()(auto ref const Value v) @trusted @nogc pure const {
        final switch (type) {
        case Type.Null:
            return type == v.type;
        case Type.Path: // TODO: compare contexts
        case Type.String:
            return type == v.type && s == v._string;
        case Type.Int:
            return i == v._number;
        case Type.Float:
            return f == v._number;
        case Type.Bool:
            return type == v.type && b == v.b;
        case Type.App:
        case Type.List:
            return type == v.type && arrayPtrEquals(l, v.l);
        case Type.Attrs:
            if (type != v.type || a.length != v.a.length) return false;
            foreach (k, p; a) {
                assert(p);
                auto pp = k in v.a;
                if (pp is null) return false;
                assert(*pp);
                if (**pp != *p) return false;
            }
            return true;
        case Type.PrimOp:
        case Type.PrimOpApp:
        case Type.Lambda:
            return false;
        case Type.Thunk:
            assert(0, "Should forceValue before calling opEquals");
        }
    }

    string toString(int depth = 1) const pure @trusted {
        final switch (type) {
        case Type.Null:
            return "null";
        case Type.Path:
            return p;
        case Type.String:
            import nix.printer : escapeString;
            return escapeString(s);
        case Type.Int:
            return text(i);
        case Type.Float:
        version (PURE) {
            import std.format : printFloat, singleSpec;
            import std.string : strip;
            static immutable fs = singleSpec("%e");
            char[32] buf;
            return printFloat(buf[], f, fs).strip("e+0").idup;
        } else {
            return text(f);
        }
        case Type.Bool:
            return b ? "true" : "false";
        case Type.List:
            if (!depth) return "[…]";
            auto s = "[ ";
            foreach (e; l) s ~= e.toString(depth - 1) ~ ' ';
            return s ~ ']';
        case Type.Attrs:
            if (!depth) return "{…}";
            auto s = "{ ";
            // FIXME: detect infinite recursion
            foreach (k, v; a) s ~= k ~ " = " ~ v.toString(depth - 1) ~ "; ";
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
            return hashOf(p);
        case Type.String:
            size_t h = hashOf(s.raw, 0x46FEB33A); // TODO hash the context as well
            try { // map iteration can throw?
                foreach (path; s.context) h ^= hashOf(path);
            } catch (Exception e) {}
            return h;
        case Type.Int:
        case Type.Float:
            return hashOf(_number);
        case Type.Bool:
            return b ? 0xCA1C5848 : 0x742D4705;
        case Type.List:
        case Type.App:
            auto h = hashOf(type);
            foreach (i, p; l) h ^= hashOf(*p, i);
            return h;
        case Type.Attrs:
            size_t h = 0;
            try { // map iteration can throw?
                // Don't care about the order of the keys
                foreach (k, p; a) h ^= hashOf(k, p.toHash());
            } catch (Exception e) {}
            return h;
        case Type.PrimOp:
        case Type.PrimOpApp:
        case Type.Lambda:
            assert(0, "Can't hash "~typeOf(this));
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
    assert(Value([new Value(3), new Value()]) == Value([new Value(3.0), new Value()]));
    assert(Value(["n":new Value(4)]) == Value(["n":new Value(4.0)]));
    assert(Value(["n":new Value()]) != Value(["k":new Value()]));
}

unittest {
    assert(Value().toString() == "null");
    assert(Value(2).toString() == "2");
    assert(Value(2.5).toString() == "2.5");
    assert(Value("hello\n", null).toString() == `"hello\n"`);
    assert(Value(true).toString() == "true");
    assert(Value(false).toString() == "false");
    assert(Value([new Value()]).toString() == "[ null ]");
    assert(Value(["n":new Value()]).toString() == "{ n = null; }");
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
    assert(Value("/") + Value("str/", null) == Value("/str"));

    assert(Value(3) < Value(12));
    assert(Value(1.0) < Value(2));
    assert(Value(2) <= Value(2.0));
    assert(Value(1.0) <= Value(2.0));
    assert(Value("/a") < Value("/b"));
    assert(Value("/a", null) < Value("/b", null));
}

unittest {
    auto functor = new Value(["__functor": new Value()]);
    const map = [
        Value(): true,
        Value("/"): true,
        Value("/string", null): true,
        Value(2): true,
        Value(3.0): true,
        Value(true): true,
        Value([new Value()]): true,
        Value(["null": new Value()]): true,
        Value(functor, new Value(2)): true,
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
    assert(Value([new Value()]) in map);
    assert(Value([new Value(1)]) !in map);
    assert(Value(["null": new Value()]) in map);
    assert(Value(["null": new Value("/")]) !in map);
    assert(Value(["asdf": new Value()]) !in map);
    assert(Value(functor, new Value(2)) in map);
    assert(Value([functor, new Value(2)]) !in map);
    assert(Value(functor, new Value(2.0)) in map);
    assert(Value(functor, new Value(42)) !in map);
    assert(Value(new Value(["attr": new Value()]), new Value(2)) !in map);
}