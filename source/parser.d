module nix.parser;

debug(PARSER) import std.stdio : writeln;
import std.conv : text, to;
import nix.printer : format;
import nix.assoc;

public import nix.lexer;

alias NixInt = long;
alias NixFloat = double;
alias Ident = string;

struct AttrName {
    Ident ident;
    Expr expr;
    this(Ident i) pure nothrow {
        assert(i);
        this.ident = i;
    }

    this(Expr e) pure nothrow {
        assert(e);
        this.expr = e;
    }

    string toString() pure const {
        return ident ? ident : (`"${` ~ expr.toString() ~ `}"`);
    }
}

alias AttrPath = AttrName[];

interface Visitor {}
interface ConstVisitor : Visitor {}

template VisitorT(T...) {
    static if (T.length > 1) {
    interface VisitorT : VisitorT!(T[0]), VisitorT!(T[1..$]) {}
    } else {
    interface VisitorT : Visitor {
        void visit(T[0]);
    }
    }
}

// interface Visitors : VisitorT!(ExprNop, ExprOpNot, ExprBinaryOp, ExprInt, ExprFloat, ExprString, ExprPath, ExprVar,
//                                ExprSelect, ExprOpHasAttr, ExprAttrs, ExprList, ExprLambda, ExprLet, ExprWith, ExprIf,
//                                ExprAssert) {
// }

template ConstVisitorT(T...) {
    static if (T.length > 1) {
    interface ConstVisitorT : ConstVisitorT!(T[0]), ConstVisitorT!(T[1..$]) {}
    } else {
    interface ConstVisitorT : VisitorT!(const T[0]), ConstVisitor {}
    }
}

interface ConstVisitors : ConstVisitorT!(ExprNop, ExprOpNot, ExprBinaryOp, ExprInt, ExprFloat, ExprString, ExprPath, ExprVar,
                               ExprSelect, ExprOpHasAttr, ExprAttrs, ExprList, ExprLambda, ExprLet, ExprWith, ExprIf,
                               ExprAssert) {
}

template Accept() {
    override void accept(Visitor v) {
        auto visitorT = cast(VisitorT!(typeof(this))) v;
        return visitorT ? visitorT.visit(this) : super.accept(v);
    }
    override void accept(Visitor v) const {
        auto visitorT = cast(VisitorT!(typeof(this))) v;
        return visitorT ? visitorT.visit(this) : super.accept(v);
    }
}

abstract class Visitable {
    void accept(Visitor v) { const t = this; t.accept(v); }
    void accept(Visitor) const { assert(0, "No visit method found for "~this.classinfo.name); }
}

abstract class Expr : Visitable {
    Loc loc;

    this(Loc loc) pure nothrow {
        this.loc = loc;
    }

    mixin Accept;
    override string toString() pure const { return "…"; }
}

class ExprNop : Expr {
    Expr expr;
    this(Loc loc, Expr expr) pure nothrow {
        assert(expr);
        super(loc);
        this.expr = expr;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
    override string toString() pure const { return "("~expr.toString()~")"; }
}

class ExprOpNot : Expr {
    Expr expr;
    this(Loc loc, Expr expr) pure nothrow {
        assert(expr);
        super(loc);
        this.expr = expr;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
    override string toString() pure const { return "!"~expr.toString(); }
}

abstract class BinaryExpr : Expr {
    Expr left;
    this(Loc loc) pure nothrow {
        super(loc);
    }
    @property abstract Tok operator() nothrow const pure;
}

class ExprBinaryOp : BinaryExpr {
    Tok op;
    Expr right;
    this(Loc loc, Tok op, Expr left, Expr right) pure nothrow {
        assert(left);
        assert(right);
        super(loc);
        this.op = op;
        this.left = left;
        this.right = right;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
    override Tok operator() nothrow const pure { return op; }
}

class ExprSelect : BinaryExpr {
    AttrPath ap;
    Expr def;
    this(Loc loc, Expr left, AttrPath ap, Expr def = null) pure nothrow {
        assert(left);
        assert(ap);
        super(loc);
        this.left = left;
        this.ap = ap;
        this.def = def;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
    override Tok operator() nothrow const pure { return Tok.SELECT; }
}

class ExprOpHasAttr : BinaryExpr {
    AttrPath ap;
    this(Loc loc, Expr left, AttrPath ap) pure nothrow {
        assert(left);
        assert(ap);
        super(loc);
        this.left = left;
        this.ap = ap;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
    override Tok operator() nothrow const pure { return Tok.HAS_ATTR; }
}

abstract class ValueExpr : Expr {
    this(Loc loc) pure nothrow {
        super(loc);
    }
}

class ExprInt : ValueExpr {
    NixInt n;
    this(Loc loc, NixInt n) pure nothrow {
        super(loc);
        this.n = n;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
}

class ExprFloat : ValueExpr {
    NixFloat f;
    this(Loc loc, NixFloat f) pure nothrow {
        super(loc);
        this.f = f;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
}

class ExprString : ValueExpr {
    string s;
    //context c;
    this(Loc loc, string s) pure nothrow {
        super(loc);
        this.s = s;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
}

class ExprPath : ValueExpr {
    string p;
    this(Loc loc, string p) pure nothrow {
        super(loc);
        this.p = p;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
}

class ExprVar : Expr {
    Ident name;
    // int level;
    //uint displ; // displacement
    this(Loc loc, Ident name) pure nothrow {
        assert(name);
        super(loc);
        this.name = name;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
}

class ExprAttrs : Expr {
    static struct AttrDef {
        Expr value;
        bool inherited;
        // Loc loc;
    }
    AttrDef[Ident] attrs;

    static struct DynamicAttrDef {
        Expr name;
        Expr value;
        // Loc loc;
    }
    DynamicAttrDef[] dynamicAttrs;
    bool recursive;
    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;

    this(Loc loc) pure nothrow {
        super(loc);
    }
}

class ExprList : Expr {
    Expr[] elems;
    this(Loc loc, Expr[] elems) pure nothrow {
        super(loc);
        this.elems = elems;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
}

struct Formal {
    Ident name;
    Expr def;
    // Loc loc;
}

class Formals {
    Formal[] elems;
    bool ellipsis;
}

class ExprLambda : Expr {
    Ident arg;
    Formals formals;
    Expr body;
    this(Loc loc, Expr body, Ident arg, Formals formals) pure nothrow {
        assert(body);
        assert(arg || formals);
        super(loc);
        this.arg = arg;
        this.formals = formals;
        this.body = body;
    }

    @property
    bool matchAttrs() pure { return formals !is null; }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
}

class ExprLet : Expr {
    ExprAttrs attrs;
    Expr body;
    this(Loc loc, ExprAttrs attrs, Expr body) pure nothrow {
        assert(attrs);
        assert(body);
        super(loc);
        this.attrs = attrs;
        this.body = body;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
}

class ExprWith : Expr {
    Expr attrs, body;
    // uint prevWith;
    this(Loc loc, Expr attrs, Expr body) pure nothrow {
        assert(attrs);
        assert(body);
        super(loc);
        this.attrs = attrs;
        this.body = body;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
}

class ExprIf : Expr {
    Expr cond, then, else_;
    this(Loc loc, Expr cond, Expr then, Expr else_) pure nothrow {
        assert(cond);
        assert(then);
        assert(else_);
        super(loc);
        this.cond = cond;
        this.then = then;
        this.else_ = else_;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
}

class ExprAssert : Expr {
    Expr cond, body;
    this(Loc loc, Expr cond, Expr body) pure nothrow {
        assert(cond);
        assert(body);
        super(loc);
        this.cond = cond;
        this.body = body;
    }

    //override void accept(Visitor v) { v.visit(this); }
    mixin Accept;
}

private ExprList parseList(R)(ref R input) pure if (isTokenRange!R) {
    Expr[] elems;
    const loc = input.front.loc;
    while (input.front.tok != Tok.RIGHT_BRACKET) {
        elems ~= parseSelect(input);
    }
    return new ExprList(loc, elems);
}

private AttrPath parseAttrs(R)(ref R input) pure if (isTokenRange!R) {
    AttrPath ap;
    while (true) {
        auto an = parseAttr(input);
        if (an.ident == "" && an.expr is null)
            break;
        ap ~= an;
    }
    return ap;
}

private ExprAttrs parseBinds(R)(ref R input) pure if (isTokenRange!R) {
    if (input.empty) return null;
    auto binds = new ExprAttrs(input.front.loc);
    while (!input.empty) {
        switch (input.front.tok) {
        case Tok.INHERIT:
            input.popFront(); // eat the inherit
            if (input.front.tok == Tok.LEFT_PARENS) {
                input.popFront(); // eat the (
                auto expr = parseExpression(input);
                enforce(input.front.tok == Tok.RIGHT_PARENS, "syntax error, unexpected '"~input.front.s~"', expecting ')'");
                input.popFront(); // eat the )
                const loc = input.front.loc;
                foreach (ap; parseAttrs(input)) {
                    enforce(ap.ident, "dynamic attributes not allowed in inherit");
                    enforceAttr(ap.ident !in binds.attrs, [ap]);
                    // FIXME: use loc from AttrPath
                    binds.attrs[ap.ident] = ExprAttrs.AttrDef(
                        new ExprSelect(loc, expr, [ap]),
                        true); // inherited
                }
            } else {
                const loc = input.front.loc;
                foreach (ap; parseAttrs(input)) {
                    enforce(ap.ident, "dynamic attributes not allowed in inherit");
                    enforceAttr(ap.ident !in binds.attrs, [ap]);
                    // FIXME: use loc from AttrPath
                    binds.attrs[ap.ident] = ExprAttrs.AttrDef(
                        new ExprVar(loc, ap.ident),
                        true); // inherited
                }
            }
            break;
        case Tok.IN:
        case Tok.RIGHT_CURLY:
            return binds;
        case Tok.OR_KW: // or=
        case Tok.IDENTIFIER: // a=
        case Tok.DOLLAR_CURLY: // ${
        case Tok.STRING: // "a
            auto ass = input.save();
            auto ap = parseAttrPath(ass);
            if (ass.front.tok != Tok.ASSIGN)
                goto default;
            assert(ap.length, input.front.s);
            input = ass; // fast forward
            enforce(input.front.tok == Tok.ASSIGN, "syntax error, unexpected '"~input.front.s~"', expecting '='");
            input.popFront(); // eat the =
            auto expr = parseExpression(input);
            assert(expr, input.front.s);
            binds.addAttr(ap, expr);
            break;
        default:
            return null;
        }
        enforce(input.front.tok == Tok.SEMICOLON, "syntax error, unexpected '"~input.front.s~"', expecting ';'");
        input.popFront(); // eat the ;
    }
    // assert(0);
    return binds;
}

unittest {
    auto parse(string s) {
        auto tr = TokenRange!string(s);
        return parseBinds(tr);
    }

    // assert(parse("").attrs.length == 0);
    assert(parse("a=2;").attrs.length == 1);
    assert(parse("a.b=2;").attrs.length == 1);
    // assert(parse(`"a"=2;`).attrs.length == 1);
    assert(parse("inherit a;").attrs.length == 1);
    assert(parse("inherit (x) a;").attrs.length == 1);
    assert(parse("or=2;").attrs.length == 1);
    assert(parse("false=2;").attrs.length == 1);
}

private bool isEmptyLine(string line) pure @safe {
    import std.algorithm : all;
    import std.ascii : isWhite;
    return line.all!isWhite;
}

unittest {
    static assert(!isEmptyLine(" a"));
    static assert(isEmptyLine("  \n"));
    static assert(isEmptyLine(""));
}

private Expr parseSimple(R)(ref R input) pure if (isTokenRange!R) {
    const t = input.front;
    switch (t.tok) {
    case Tok.LET:
        input.popFront(); // eat the let
        enforce(input.front.tok == Tok.LEFT_CURLY, "syntax error, unexpected '"~input.front.s~"', expecting '{'");
        input.popFront(); // eat the {
        /* Let expressions `let {..., body = ...}' are just desugared
           into `(rec {..., body = ...}).body'. */
        auto binds = parseBinds(input);
        enforce(input.front.tok == Tok.RIGHT_CURLY, "syntax error, unexpected '"~input.front.s~"', expecting '}'");
        input.popFront(); // eat the }
        binds.recursive = true;
        return new ExprSelect(t.loc, binds, [AttrName("body")]);
    case Tok.REC:
        input.popFront(); // eat the rec
        enforce(input.front.tok == Tok.LEFT_CURLY, "syntax error, unexpected '"~input.front.s~"', expecting '{'");
        input.popFront(); // eat the {
        auto binds = parseBinds(input);
        enforce(input.front.tok == Tok.RIGHT_CURLY, "syntax error, unexpected '"~input.front.s~"', expecting '}'");
        input.popFront(); // eat the }
        binds.recursive = true;
        return binds;
    case Tok.LEFT_CURLY: // {} or {a= or {${ or {" or {inherit or {or=
        auto b = input.save();
        b.popFront(); // eat the {
        auto binds = parseBinds(b);
        if (!binds)
            return null;
        input = b; // fast forward
        enforce(input.front.tok == Tok.RIGHT_CURLY, "syntax error, unexpected '"~input.front.s~"', expecting '}'");
        input.popFront(); // eat the }
        return binds;
    case Tok.IDENTIFIER:
        input.popFront();
        if (t.s == "__curPos") {
            auto pos = new ExprAttrs(t.loc);
            // pos.attrs["file"] = ExprAttrs.AttrDef(new ExprString(t.loc, t.loc.file));
            pos.attrs["line"] = ExprAttrs.AttrDef(new ExprInt(t.loc, t.loc.line));
            // pos.attrs["column"] = ExprAttrs.AttrDef(new ExprInt(t.loc, t.loc.column));
            return pos;
        }
        return new ExprVar(t.loc, t.s);
    case Tok.LEFT_BRACKET:
        input.popFront(); // eat the [
        auto e = parseList(input);
        enforce(input.front.tok == Tok.RIGHT_BRACKET, "syntax error, unexpected '"~input.front.s~"', expecting ']'");
        input.popFront(); // eat the ]
        return e;
    case Tok.LEFT_PARENS:
        input.popFront(); // eat the (
        auto e = parseExpression(input);
        debug(PARSER) writeln("parseSimple: parseExpression returned ", format(e));
        assert(e, input.front.s);
        enforce(input.front.tok == Tok.RIGHT_PARENS, "syntax error, unexpected '"~input.front.s~"', expecting ')'");
        input.popFront(); // eat the )
        // Wrap the resulting expression in a NOP to avoid assoc reshuffling
        return new ExprNop(t.loc, e);
    case Tok.INT:
        input.popFront();
        return new ExprInt(t.loc, to!NixInt(t.s));
    case Tok.FLOAT:
        input.popFront();
        return new ExprFloat(t.loc, to!NixFloat(t.s));
    case Tok.URI: // deprecated!
        input.popFront();
        return new ExprString(t.loc, t.s);
    case Tok.PATH:
    case Tok.HPATH:
        input.popFront();
        // Cannot convert paths to absolute here because parsing is pure
        return new ExprPath(t.loc, t.s);
    case Tok.SPATH:
        input.popFront();
        // TODO: consider creating a ExprSPath AST node instead of rewriting during parsing
        auto findNixPath = new ExprBinaryOp(t.loc, Tok.APP, new ExprVar(t.loc, "__findFile"), new ExprVar(t.loc, "__nixPath"));
        return new ExprBinaryOp(t.loc, Tok.APP, findNixPath, new ExprString(t.loc, t.s));
    case Tok.STRING_OPEN:
        input.popFront(); // eat the "
        return parseStr(input);
    case Tok.STRING:
        auto tokens = parseString(input.front.s);
        input.popFront();
        assert(tokens.front.tok == Tok.STRING_OPEN);
        tokens.popFront(); // eat the "
        return parseStr(tokens);
    case Tok.IND_STRING:
        assert(input.front.s[0..2] == "''");
        assert(input.front.s[$-2..$] == "''");
        import std.algorithm : filter, map, minElement, countUntil, splitter;
        import std.array : join, array;
        auto lines = input.front.s[2..$-2].splitter('\n').array;
        input.popFront();
        if (lines.length && lines[0].isEmptyLine) lines = lines[1..$];
        if (lines.length && lines[$-1].isEmptyLine) lines[$-1] = "";
        const indentation = lines
            .filter!(line => !isEmptyLine(line))
            .map!(line => line.countUntil!"a!=b"(' '))
            .minElement(int.max) % int.max;
        if (indentation > 0) {
            foreach (ref line; lines) {
                line = line.length > indentation ? line[indentation..$] : "";
            }
        }
        auto text = lines.join('\n') ~ "''";
        auto tokens = parseIndString(text, false);
        return parseStr(tokens);
    default:
        return null;
    }
}

unittest {
    auto parse(string s) {
        auto tr = TokenRange!string(s);
        return parseSimple(tr);
    }

    with(cast(ExprVar) parse("null")) { assert(name == "null"); }
    with(cast(ExprVar) parse("true")) { assert(name == "true"); }
    with(cast(ExprVar) parse("false")) { assert(name == "false"); }
    with(cast(ExprVar) parse("a")) { assert(name == "a"); }
    with(cast(ExprInt) parse("2")) { assert(n == 2); }
    with(cast(ExprNop) parse("(2)")) { assert(cast(ExprInt) expr); }
    with(cast(ExprFloat) parse("2.")) { assert(f == 2.0); }
    with(cast(ExprString) parse(`"str"`)) { assert(s == "str"); }
    with(cast(ExprString) parse(`''\n str''`)) { assert(s == "\\n str"); }
    with(cast(ExprString) parse("http://a.com")) { assert(s == "http://a.com"); }
    with(cast(ExprPath) parse("/a")) { assert(p == "/a"); }
    with(cast(ExprPath) parse("./a")) { assert(p == "./a"); }
    with(cast(ExprPath) parse("~/a")) { assert(p == "~/a"); }
    with(cast(ExprBinaryOp) parse("<a>")) { assert(op == Tok.APP); }
    with(cast(ExprVar) parse("libcap/* bla*/")) { assert(name == "libcap"); }
    with(cast(ExprBinaryOp) parse(`"4n${"2"}"`)) { assert(op == Tok.ADD); }
    with(cast(ExprBinaryOp) parse(`''4n${"2"}''`)) { assert(op == Tok.ADD); }
}

private Expr parseStr(R)(ref R input) pure if (isTokenRange!R) {
    Expr[] es;
    const EOF = -1;
    while (true) {
        switch(input.empty ? EOF : input.front.tok) {
        case Tok.STR:
            es ~= new ExprString(input.front.loc, input.front.s);
            input.popFront(); // eat the str
            break;
        case EOF:
        case Tok.IND_STRING_CLOSE:
        case Tok.STRING_CLOSE:
            const loc = input.front.loc;
            input.popFront(); // eat the " or ''
            if (es.length == 1 && cast(ExprString)es[0]) return es[0];
            // Always start with an empty string to force string coercion
            Expr concat = new ExprString(loc, "");
            foreach (e; es[0..$]) {
                concat = new ExprBinaryOp(e.loc, Tok.ADD, concat, e);
            }
            return concat;
        case Tok.DOLLAR_CURLY:
            input.popFront(); // eat the ${
            auto expr = parseExpression(input);
            if (expr) es ~= expr;
            debug(PARSER) writeln(format(expr));
            enforce(input.front.tok == Tok.RIGHT_CURLY, "syntax error, unexpected "~input.front.s~", expecting '}'");
            input.popFront(); // eat the }
            break;
        default:
            assert(0);
        }
    }
}

unittest {
    auto r = [
        // Token(Tok.STRING_OPEN),
        Token(Tok.DOLLAR_CURLY),
        Token(Tok.INT, "2"),
        Token(Tok.RIGHT_CURLY),
        Token(Tok.STRING_CLOSE)
    ];
    assert(cast(ExprBinaryOp) parseStr(r));
    assert(r.empty);
}

private Formals parseFormals(R)(ref R input) pure if (isTokenRange!R) {
    auto f = new Formals;
    while (true) {
        switch (input.front.tok) {
        case Tok.IDENTIFIER:
            const id = input.front.s;
            input.popFront(); // eat the id
            switch (input.front.tok) {
            case Tok.HAS_ATTR:
                input.popFront(); // eat the ?
                auto expr = parseExpression(input);
                assert(expr, input.front.s);
                f.elems ~= Formal(id, expr);
                if (input.front.tok == Tok.COMMA)
                    input.popFront();
                continue;
            case Tok.COMMA:
                input.popFront(); // eat the ,
                f.elems ~= Formal(id);
                continue;
            default:
                break;
            }
            f.elems ~= Formal(id);
            break;
        case Tok.ELLIPSIS:
            input.popFront(); // eat the ...
            f.ellipsis = true;
            break;
        default:
            return f;
        }
    }
}

unittest {
    auto parse(string s) {
        auto tr = TokenRange!string(s);
        return parseFormals(tr);
    }

    assert(!parse("").ellipsis);
    assert(parse("...").ellipsis);
    assert(parse("a").elems == [Formal("a")]);
    assert(parse("a,b").elems == [Formal("a"), Formal("b")]);
}

/// Parse the tokens from the given range into an expression tree.
public alias parse = parseExpression;

/// Ditto
public Expr parse(C)(immutable(C)[] s) pure if (is(C : dchar)) {
    auto tr = TokenRange!(immutable(C)[])(s);
    return parseExpression(tr);
}

/// Ditto
private Expr parseExpression(R)(ref R input) pure if (isTokenRange!R) {
    debug(PARSER) scope(exit) writeln("parseExpression <- ");
    const t = input.front;
    switch (t.tok) {
    case Tok.IDENTIFIER: // args@ or args: or func val or set.attr
        auto lambda = input.save();
        lambda.popFront(); // eat the id
        switch (lambda.front.tok) {
        case Tok.AT:
            input.popFront(); // eat the id
            input.popFront(); // eat the @
            enforce(input.front.tok == Tok.LEFT_CURLY, "syntax error, unexpected '"~input.front.s~"', expecting '{'");
            input.popFront(); // eat the {
            auto formals = parseFormals(input);
            assert(formals, input.front.s);
            enforce(input.front.tok == Tok.RIGHT_CURLY, "syntax error, unexpected '"~input.front.s~"', expecting '}'");
            input.popFront(); // eat the }
            enforce(input.front.tok == Tok.COLON, "syntax error, unexpected '"~input.front.s~"', expecting ':'");
            input.popFront(); // eat the :
            auto body = parseExpression(input);
            assert(body, input.front.s);
            return new ExprLambda(t.loc, body, t.s, formals);
        case Tok.COLON:
            input.popFront(); // eat the id
            input.popFront(); // eat the :
            auto body = parseExpression(input);
            assert(body, input.front.s);
            return new ExprLambda(t.loc, body, t.s, null);
        default:
            break;
        }
        goto default;
    case Tok.LEFT_CURLY: // {} or {a} or {a? or {a, or {...
        auto f = input.save();
        f.popFront(); // eat the {
        auto formals = parseFormals(f);
        assert(formals, input.front.s);
        if (f.front.tok != Tok.RIGHT_CURLY)
            goto default;
        f.popFront(); // eat the }
        switch (f.front.tok) {
        case Tok.AT:
            input = f; // fast forward
            input.popFront(); // eat the @
            const id = input.front.s;
            enforce(input.front.tok == Tok.IDENTIFIER, "syntax error, unexpected '"~input.front.s~"', expecting an identifier");
            input.popFront(); // eat the id
            enforce(input.front.tok == Tok.COLON, "syntax error, unexpected '"~input.front.s~"', expecting ':'");
            input.popFront(); // eat the :
            auto body = parseExpression(input);
            assert(body, input.front.s);
            return new ExprLambda(t.loc, body, id, formals);
        case Tok.COLON:
            input = f; // fast forward
            input.popFront(); // eat the :
            auto body = parseExpression(input);
            assert(body, input.front.s);
            return new ExprLambda(t.loc, body, null, formals);
        default:
            // assert(0, input.front.s);
            break;
        }
        goto default;
    case Tok.ASSERT:
        input.popFront(); // eat the assert
        auto expr = parseExpression(input);
        assert(expr, input.front.s);
        enforce(input.front.tok == Tok.SEMICOLON, "syntax error, unexpected '"~input.front.s~"', expecting ';'");
        input.popFront(); // eat the ;
        return new ExprAssert(t.loc, expr, parseExpression(input));
    case Tok.WITH:
        input.popFront(); // eat the with
        auto expr = parseExpression(input);
        assert(expr, input.front.s);
        enforce(input.front.tok == Tok.SEMICOLON, "syntax error, unexpected '"~input.front.s~"', expecting ';'");
        input.popFront(); // eat the ;
        return new ExprWith(t.loc, expr, parseExpression(input));
    case Tok.LET:
        auto let = input.save();
        let.popFront(); // eat the let
        if (let.front.tok == Tok.LEFT_CURLY)
            goto default;
        input = let; // fast forward
        auto binds = parseBinds(input);
        assert(binds, input.front.s);
        assert(0 == binds.dynamicAttrs.length); // dynamic attributes not allowed here
        enforce(input.front.tok == Tok.IN, "syntax error, unexpected '"~input.front.s~"', expecting 'in'");
        input.popFront(); // eat the in
        return new ExprLet(t.loc, binds, parseExpression(input));
    default:
        return parseIf(input);
    }
}

private Expr parseIf(R)(ref R input) pure if (isTokenRange!R) {
    debug(PARSER) scope(exit) writeln("parseIf <- ");
    const loc = input.front.loc;
    switch (input.front.tok) {
    case Tok.IF:
        input.popFront(); // eat the if
        auto cond = parseExpression(input);
        assert(cond, input.front.s);
        enforce(input.front.tok == Tok.THEN, "syntax error, unexpected '"~input.front.s~"', expecting 'then'");
        input.popFront(); // eat the then
        auto then = parseExpression(input);
        assert(then, input.front.s);
        enforce(input.front.tok == Tok.ELSE, "syntax error, unexpected '"~input.front.s~"', expecting 'else'");
        input.popFront(); // eat the else
        auto else_ = parseExpression(input);
        assert(else_, input.front.s);
        return new ExprIf(loc, cond, then, else_);
    default:
        return parseOp(input);
    }
}

private Expr parseOneOp(R)(ref R input) pure if (isTokenRange!R) {
    debug(PARSER) writeln("parseOneOp ", input.front.tok, input.front.s);
    debug(PARSER) scope(exit) writeln("parseOneOp <- ");
    switch (input.front.tok) {
    case Tok.NOT:
        const loc = input.front.loc;
        input.popFront(); // eat the !
        auto expr = parseOneOp(input);
        debug(PARSER) writeln("parseOneOp: parseOneOp returned ", format(expr));
        return new ExprOpNot(loc, expr);
    case Tok.NEGATE:
        const loc = input.front.loc;
        input.popFront(); // eat the -
        auto expr = parseOneOp(input);
        debug(PARSER) writeln("parseOneOp: parseOneOp returned ", format(expr));
        return new ExprBinaryOp(loc, Tok.NEGATE, new ExprInt(loc, 0), expr);
    default:
        auto left = parseApp(input);
        debug(PARSER) writeln("parseOneOp: parseApp returned ", format(left));
        if (!left || input.empty)
            return left;
        if (input.front.tok == Tok.HAS_ATTR) {
            const loc = input.front.loc;
            input.popFront(); // eat the ?
            auto ap = parseAttrPath(input);
            assert(ap, input.front.s);
            assert(input.front.tok != Tok.HAS_ATTR, "TODO handle this");
            return new ExprOpHasAttr(loc, left, ap);
        }
        return left;
    }
}

unittest {
    auto parse(string s) {
        auto tr = TokenRange!string(s);
        auto e = parseOneOp(tr);
        return e;
    }

    with(cast(ExprVar) parse("a + b")) { assert(name == "a"); }
    with(cast(ExprOpNot) parse("!a + b")) { assert(cast(ExprVar) expr); }
    with(cast(ExprOpNot) parse("!!a + b")) { assert(cast(ExprOpNot) expr); }
    with(cast(ExprOpHasAttr) parse("a ? a + b")) { assert(cast(ExprVar) left); }
    // with(cast(ExprOpHasAttr) parse("a ? a ? a + b")) { assert(cast(ExprOpHasAttr) left); }
}

private Expr parseOp(R)(ref R input) pure if (isTokenRange!R) {
    debug(PARSER) writeln("parseOp ", input.front.tok, input.front.s);
    debug(PARSER) scope(exit) writeln("parseOp <- ");
    auto left = parseOneOp(input);
    debug(PARSER) writeln("parseOp: parseOneOp returned ", format(left));
    if (!left || input.empty)
        return left;
L_next:
    auto op = input.front.tok;
    switch (op) {
    case Tok.NEGATE:
        // Lexer doesn't know the difference between NEGATE and SUB
        op = Tok.SUB;
        goto case Tok.SUB;
    case Tok.SUB:
    case Tok.CONCAT:
    case Tok.MUL:
    case Tok.DIV:
    case Tok.ADD:
    case Tok.UPDATE:
    case Tok.LT:
    case Tok.LEQ:
    case Tok.GT:
    case Tok.GEQ:
    case Tok.EQ:
    case Tok.NEQ:
    case Tok.AND:
    case Tok.OR:
    case Tok.IMPL:
        const loc = input.front.loc;
        input.popFront(); // eat the op
        auto right = parseOneOp(input);
        if (!right)
            return left;
        if (input.empty)
            return new ExprBinaryOp(loc, op, left, right);
        auto op2 = input.front.tok;
        switch (op2) {
        case Tok.NEGATE:
            // Lexer doesn't know the difference between NEGATE and SUB
            op2 = Tok.SUB;
            goto case Tok.SUB;
        case Tok.SUB:
        case Tok.CONCAT:
        case Tok.MUL:
        case Tok.DIV:
        case Tok.ADD:
        case Tok.UPDATE:
        case Tok.LT:
        case Tok.LEQ:
        case Tok.GT:
        case Tok.GEQ:
        case Tok.EQ:
        case Tok.NEQ:
        case Tok.AND:
        case Tok.OR:
        case Tok.IMPL:
            const assoc = associativity(op, op2);
            enforce(assoc != Associativity.NONE, "Non-associative");
            if (assoc == Associativity.LEFT) {
                left = new ExprBinaryOp(loc, op, left, right);
                goto L_next;
            } else {
                const loc2 = input.front.loc;
                input.popFront(); // eat the op2
                left = new ExprBinaryOp(loc, op, left, new ExprBinaryOp(loc2, op2, right, parseOneOp(input)));
                goto L_next;
            }
        default:
            return new ExprBinaryOp(loc, op, left, right);
        }
    default:
        return left;
    }
}

unittest {
    auto parse(string s) {
        auto tr = TokenRange!string(s);
        auto e = parseOp(tr);
        assert(tr.empty);
        return e;
    }

    with(cast(ExprBinaryOp) parse("a + b")) { assert(op == Tok.ADD); }
    with(cast(ExprBinaryOp) parse("a && b")) { assert(op == Tok.AND); }
    with(cast(ExprBinaryOp) parse("!a && b")) { assert(op == Tok.AND); }
    with(cast(ExprBinaryOp) parse("-a + b")) { assert(op == Tok.ADD); }
    with(cast(ExprBinaryOp) parse("a + b - c")) { assert(op == Tok.SUB); }
    with(cast(ExprBinaryOp) parse("a + b * c < d")) { assert(op == Tok.LT); }
    assert(cast(ExprOpHasAttr) parse("a ? b"));
    with(cast(ExprBinaryOp) parse("a ? b || c")) { assert(op == Tok.OR); }
    with(cast(ExprBinaryOp) parse("a ? b || c ? d")) { assert(op == Tok.OR); }
    with(cast(ExprBinaryOp) parse("2 * 3 + 4")) { assert(op == Tok.ADD); }
    with(cast(ExprBinaryOp) parse("2 + 3 * 4")) { assert(op == Tok.ADD); }
    with(cast(ExprBinaryOp) parse("2 * 3 + 4 * 4")) { assert(op == Tok.ADD); }
    with(cast(ExprBinaryOp) parse("2 * 3 * 4 + 4")) { assert(op == Tok.ADD); }
    with(cast(ExprBinaryOp) parse("10 - 6 - -1")) { assert(cast(ExprBinaryOp) left); }
    with(cast(ExprBinaryOp) parse("(10 - 6) - -1")) { assert(cast(ExprNop) left); }
    with(cast(ExprBinaryOp) parse("10 - (6 - -1)")) { assert(cast(ExprInt) left); }
    import std.exception: assertThrown;
    assertThrown(parse("2 == 3 == 4"));
    assertThrown(parse("2 != 3 != 4"));
}

private Expr parseApp(R)(ref R input) pure if (isTokenRange!R) {
    debug(PARSER) writeln("parseApp ", input.front.tok, input.front.s);
    auto select = parseSelect(input);
    debug(PARSER) writeln("parseApp: parseSelect returned ", format(select));
    if (!select)
        return null;
    while (true) {
        const loc = input.front.loc;
        auto arg = parseSelect(input);
        if (!arg)
            break;
        debug(PARSER) writeln("parseApp: parseSelect returned ", format(arg));
        select = new ExprBinaryOp(loc, Tok.APP, select, arg);
    }
    return select;
}

unittest {
    auto parse(string s) {
        auto tr = TokenRange!string(s);
        return parseApp(tr);
    }

    assert(parse("a"));
    assert(parse("a b"));
    assert(parse("a b.c"));
    assert(parse("a.b c"));
}

private Expr parseSelect(R)(ref R input) pure if (isTokenRange!R) {
    auto arg = parseSimple(input);
    const loc = input.front.loc;
    switch (input.front.tok) {
    case Tok.SELECT:
        assert(arg);
        input.popFront(); // eat the .
        auto attrpath = parseAttrPath(input);
        if (input.front.tok == Tok.OR_KW) {
            input.popFront(); // eat the or
            auto def = parseSelect(input);
            return new ExprSelect(loc, arg, attrpath, def);
        } else {
            return new ExprSelect(loc, arg, attrpath);
        }
    case Tok.OR_KW:
        assert(arg);
        input.popFront(); // eat the or
        /* Backwards compatibility: because Nixpkgs has a rarely used
            function named ‘or’, allow stuff like ‘map or [...]’. */
        return new ExprBinaryOp(loc, Tok.APP, arg, new ExprVar(loc, "or"));
    default:
        return arg;
    }
}

private AttrName parseAttr(R)(ref R input) pure if (isTokenRange!R) {
    switch (input.front.tok) {
    case Tok.DOLLAR_CURLY:
        input.popFront(); // eat the ${
        auto expr = parseExpression(input);
        enforce(input.front.tok == Tok.RIGHT_CURLY, "syntax error, unexpected '"~input.front.s~"', expecting '}'");
        input.popFront(); // eat the }
        return AttrName(expr);
    case Tok.STRING: // could contain ${}
        auto tokens = parseString(input.front.s);
        input.popFront();
        assert(tokens.front.tok == Tok.STRING_OPEN);
        tokens.popFront(); // eat the "
        auto expr = parseStr(tokens);
        auto string_expr = cast(ExprString) expr;
        return string_expr ? AttrName(string_expr.s) : AttrName(expr);
    case Tok.OR_KW:
    case Tok.IDENTIFIER:
        auto id = input.front.s;
        input.popFront(); // eat the id
        return AttrName(id);
    default:
        return AttrName.init;
    }
}

private AttrPath parseAttrPath(R)(ref R input) pure if (isTokenRange!R) {
    auto an = parseAttr(input);
    if (an.ident is null && an.expr is null)
        return null;
    assert(an.ident || an.expr);
    AttrPath ap = [an];
    while (true) {
        switch (input.front.tok) {
        case Tok.SELECT:
            input.popFront(); // eat the .
            an = parseAttr(input);
            assert(an.ident || an.expr);
            ap ~= an;
            break;
        default:
            return ap;
        }
    }
}

private void enforceAttr(T)(in T test, AttrPath ap) pure {
    string str;
    foreach (i, an; ap) {
        if (i) str ~= '.';
        str ~= an.toString();
    }
    enforce(test, "attribute '"~str~"' already defined");
}

private void addAttr(ExprAttrs attrs, AttrPath ap, Expr e) pure {
    assert(ap.length > 0);
    foreach (i; ap[0 .. $ - 1]) {
        if (i.ident) {
            const j = i.ident in attrs.attrs;
            if (j) {
                enforceAttr(!j.inherited, ap);
                attrs = cast(ExprAttrs) j.value;
                enforceAttr(attrs, ap);
            } else {
                auto nested = new ExprAttrs(e.loc);
                attrs.attrs[i.ident] = ExprAttrs.AttrDef(nested);
                attrs = nested;
            }
        } else {
            auto nested = new ExprAttrs(i.expr.loc);
            // debug writeln(__LINE__,format(i.expr));
            attrs.dynamicAttrs ~= ExprAttrs.DynamicAttrDef(i.expr, nested);
            attrs = nested;
        }
    }
    auto i = ap[$ - 1];
    if (i.ident) {
        const j = i.ident in attrs.attrs;
        if (j) {
            auto ae = cast(ExprAttrs) e;
            enforceAttr(ae, ap);
            auto jAttrs = cast(ExprAttrs) j.value;
            enforceAttr(jAttrs, ap);
            foreach (k, v; ae.attrs) {
                const j2 = k in jAttrs.attrs;
                enforceAttr(!j2, ap); // Attr already defined in jAttrs, error.
                jAttrs.attrs[k] = v;
            }
        } else {
            // This attr path is not defined. Let's create it.
            attrs.attrs[i.ident] = ExprAttrs.AttrDef(e);
        }
    } else {
        // debug writeln(__LINE__,format(i.expr));
        attrs.dynamicAttrs ~= ExprAttrs.DynamicAttrDef(i.expr, e);
    }
}
