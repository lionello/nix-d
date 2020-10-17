module nix.parser;

debug(PARSER) import std.stdio : writeln;
import std.conv : to;
import nix.printer : format;

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
}

alias AttrPath = AttrName[];

interface Visitor {
    void visit(in ExprNop);
    void visit(in ExprOpNot);
    void visit(in ExprBinaryOp);
    void visit(in ExprInt);
    void visit(in ExprFloat);
    void visit(in ExprString);
    void visit(in ExprPath);
    void visit(in ExprVar);
    void visit(in ExprSelect);
    void visit(in ExprOpHasAttr);
    void visit(in ExprAttrs);
    void visit(in ExprList);
    void visit(in ExprLambda);
    void visit(in ExprLet);
    void visit(in ExprWith);
    void visit(in ExprIf);
    void visit(in ExprAssert);
}

abstract class Expr {
    Loc loc; // TODO
    abstract void accept(Visitor v) const;
}

class ExprNop : Expr {
    Expr expr;
    this(Expr expr) pure nothrow {
        assert(expr);
        this.expr = expr;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
}

class ExprOpNot : Expr {
    Expr expr;
    this(Expr expr) pure nothrow {
        assert(expr);
        this.expr = expr;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
}

abstract class BinaryExpr : Expr {
    Expr left;
    @property abstract Tok operator() nothrow const pure;
}

class ExprBinaryOp : BinaryExpr {
    Tok op;
    Expr right;
    this(Tok op, Expr left, Expr right) pure nothrow {
        assert(left);
        assert(right);
        this.op = op;
        this.left = left;
        this.right = right;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
    override Tok operator() nothrow const pure { return op; }
}

class ExprSelect : BinaryExpr {
    AttrPath ap;
    Expr def;
    this(Expr left, AttrPath ap, Expr def = null) pure nothrow {
        assert(left);
        assert(ap);
        this.left = left;
        this.ap = ap;
        this.def = def;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
    override Tok operator() nothrow const pure { return Tok.SELECT; }
}

class ExprOpHasAttr : BinaryExpr {
    AttrPath ap;
    this(Expr left, AttrPath ap) pure nothrow {
        assert(left);
        assert(ap);
        this.left = left;
        this.ap = ap;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
    override Tok operator() nothrow const pure { return Tok.HAS_ATTR; }
}

abstract class ValueExpr : Expr {
}

class ExprInt : ValueExpr {
    NixInt n;
    this(NixInt n) pure nothrow {
        this.n = n;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
}

class ExprFloat : ValueExpr {
    NixFloat f;
    this(NixFloat f) pure nothrow {
        this.f = f;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
}

class ExprString : ValueExpr {
    string s;
    //context c;
    this(string s) pure nothrow {
        this.s = s;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
}

class ExprPath : ValueExpr {
    string p;
    this(string p) pure nothrow {
        this.p = p;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
}

class ExprVar : Expr {
    Ident name;
    // int level;
    //uint displ; // displacement
    this(Ident name) pure nothrow {
        assert(name);
        this.name = name;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
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
    override void accept(Visitor v) const {
        v.visit(this);
    }
}

class ExprList : Expr {
    Expr[] elems;
    this(Expr[] elems) pure nothrow {
        this.elems = elems;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
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
    this(Expr body, Ident arg, Formals formals) pure nothrow {
        assert(body);
        this.arg = arg;
        this.formals = formals;
        this.body = body;
    }

    @property
    bool matchAttrs() pure { return formals !is null; }

    override void accept(Visitor v) const {
        v.visit(this);
    }
}

class ExprLet : Expr {
    ExprAttrs attrs;
    Expr body;
    this(ExprAttrs attrs, Expr body) pure nothrow {
        assert(attrs);
        assert(body);
        this.attrs = attrs;
        this.body = body;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
}

class ExprWith : Expr {
    Expr attrs, body;
    // uint prevWith;
    this(Expr attrs, Expr body) pure nothrow {
        assert(attrs);
        assert(body);
        this.attrs = attrs;
        this.body = body;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
}

class ExprIf : Expr {
    Expr cond, then, else_;
    this(Expr cond, Expr then, Expr else_) pure nothrow {
        assert(cond);
        assert(then);
        assert(else_);
        this.cond = cond;
        this.then = then;
        this.else_ = else_;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
}

class ExprAssert : Expr {
    Expr cond, body;
    this(Expr cond, Expr body) pure nothrow {
        assert(cond);
        assert(body);
        this.cond = cond;
        this.body = body;
    }

    override void accept(Visitor v) const {
        v.visit(this);
    }
}

private ExprList parseList(R)(ref R input) pure if (isTokenRange!R) {
    Expr[] elems;
    while (input.front.tok != Tok.RIGHT_BRACKET) {
        elems ~= parseSelect(input);
    }
    return new ExprList(elems);
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
    auto binds = new ExprAttrs();
    while (!input.empty) {
        switch (input.front.tok) {
        case Tok.INHERIT:
            input.popFront(); // eat the inherit
            if (input.front.tok == Tok.LEFT_PARENS) {
                input.popFront(); // eat the (
                auto expr = parseExpression(input);
                assert(input.front.tok == Tok.RIGHT_PARENS, input.front.s);
                input.popFront(); // eat the )
                foreach (ap; parseAttrs(input)) {
                    assert(ap.ident);
                    assert(ap.ident !in binds.attrs);
                    binds.attrs[ap.ident] = ExprAttrs.AttrDef(new ExprSelect(expr, [
                                ap
                            ]), true);
                }
            } else {
                foreach (ap; parseAttrs(input)) {
                    assert(ap.ident);
                    assert(ap.ident !in binds.attrs);
                    binds.attrs[ap.ident] = ExprAttrs.AttrDef(new ExprVar(ap.ident), true);
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
            assert(input.front.tok == Tok.ASSIGN, input.front.s);
            input.popFront(); // eat the =
            auto expr = parseExpression(input);
            assert(expr, input.front.s);
            binds.addAttr(ap, expr);
            break;
        default:
            return null;
        }
        assert(input.front.tok == Tok.SEMICOLON, input.front.s);
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

    assert(parse("").attrs.length == 0);
    assert(parse("a=2;").attrs.length == 1);
    assert(parse("a.b=2;").attrs.length == 1);
    assert(parse(`"a"=2;`).attrs.length == 1);
    assert(parse("inherit a;").attrs.length == 1);
    assert(parse("inherit (x) a;").attrs.length == 1);
    assert(parse("or=2;").attrs.length == 1);
    assert(parse("false=2;").attrs.length == 1);
}

private Expr parseSimple(R)(ref R input) pure if (isTokenRange!R) {
    const t = input.front;
    switch (t.tok) {
    case Tok.LET:
        input.popFront(); // eat the let
        assert(input.front.tok == Tok.LEFT_CURLY, input.front.s);
        input.popFront(); // eat the {
        /* Let expressions `let {..., body = ...}' are just desugared
           into `(rec {..., body = ...}).body'. */
        auto binds = parseBinds(input);
        assert(input.front.tok == Tok.RIGHT_CURLY, input.front.s);
        input.popFront(); // eat the }
        binds.recursive = true;
        return new ExprSelect(binds, [AttrName("body")]);
    case Tok.REC:
        input.popFront(); // eat the rec
        assert(input.front.tok == Tok.LEFT_CURLY, input.front.s);
        input.popFront(); // eat the {
        auto binds = parseBinds(input);
        assert(input.front.tok == Tok.RIGHT_CURLY, input.front.s);
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
        assert(input.front.tok == Tok.RIGHT_CURLY, input.front.s);
        input.popFront(); // eat the }
        return binds;
    case Tok.IDENTIFIER:
        input.popFront();
        return new ExprVar(t.s);
    case Tok.LEFT_BRACKET:
        input.popFront(); // eat the [
        auto e = parseList(input);
        assert(input.front.tok == Tok.RIGHT_BRACKET, input.front.s);
        input.popFront(); // eat the ]
        return e;
    case Tok.LEFT_PARENS:
        input.popFront(); // eat the (
        auto e = parseExpression(input);
        debug(PARSER) writeln("parseSimple: parseExpression returned ", format(e));
        assert(e, input.front.s);
        assert(input.front.tok == Tok.RIGHT_PARENS, input.front.s);
        input.popFront(); // eat the )
        // Wrap the resulting expression in a NOP to avoid assoc reshuffling
        return new ExprNop(e);
    case Tok.INT:
        input.popFront();
        return new ExprInt(to!NixInt(t.s));
    case Tok.FLOAT:
        input.popFront();
        return new ExprFloat(to!NixFloat(t.s));
    case Tok.URI: // deprecated!
        input.popFront();
        return new ExprString(t.s);
    case Tok.PATH:
    case Tok.HPATH:
        input.popFront();
        // Cannot convert paths to absolute here because parsing is pure
        return new ExprPath(t.s);
    case Tok.SPATH:
        input.popFront();
        auto findNixPath = new ExprBinaryOp(Tok.APP, new ExprVar("__findFile"), new ExprVar("__nixPath"));
        return new ExprBinaryOp(Tok.APP, findNixPath, new ExprString(t.s));
    case Tok.STRING_OPEN:
        return parseStr(input);
    case Tok.STRING:
        auto tokens = parseString(input.front.s);
        input.popFront();
        return parseStr(tokens);
    case Tok.IND_STRING:
        // TODO: strip indentation
        auto tokens = parseIndString(input.front.s);
        input.popFront();
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

    assert(cast(ExprVar) parse("null"));
    assert(cast(ExprVar) parse("true"));
    assert(cast(ExprVar) parse("false"));
    assert(cast(ExprVar) parse("a"));
    assert(cast(ExprInt) parse("2"));
    assert(cast(ExprNop) parse("(2)"));
    assert(cast(ExprFloat) parse("2."));
    assert(cast(ExprString) parse(`"str"`));
    assert(cast(ExprString) parse(`''\n str''`));
    assert(cast(ExprString) parse("http://a.com"));
    assert(cast(ExprPath) parse("/a"));
    assert(cast(ExprPath) parse("./a"));
    assert(cast(ExprPath) parse("~/a"));
    assert(cast(ExprBinaryOp) parse("<a>"));
    assert(cast(ExprVar) parse("libcap/* bla*/"));
    assert(cast(ExprBinaryOp) parse(`"4n${"2"}"`));
    assert(cast(ExprBinaryOp) parse(`''4n${"2"}''`));
}

private Expr parseStr(R)(ref R input) pure if (isTokenRange!R) {
    assert(input.front.tok == Tok.STRING_OPEN || input.front.tok == Tok.IND_STRING_OPEN);
    input.popFront(); // eat the " or ''
    Expr[] es;
    while (true) {
        switch(input.front.tok) {
        case Tok.STR:
            es ~= new ExprString(input.front.s);
            input.popFront(); // eat the str
            break;
        case Tok.IND_STRING_CLOSE:
        case Tok.STRING_CLOSE:
            input.popFront(); // eat the " or ''
            if (es.length == 1 && cast(ExprString)es[0]) return es[0];
            // Always start with an empty string to force coercion
            Expr concat = new ExprString("");
            foreach (e; es[0..$]) {
                concat = new ExprBinaryOp(Tok.ADD, concat, e);
            }
            return concat;
        case Tok.DOLLAR_CURLY:
            input.popFront(); // eat the ${
            es ~= parseExpression(input);
            import std.conv:to;
            assert(input.front.tok == Tok.RIGHT_CURLY, to!string(input.front.tok));
            input.popFront(); // eat the }
            break;
        default:
            assert(0);
        }
    }
}

unittest {
    auto r = [
        Token(Tok.STRING_OPEN),
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

public alias parse = parseExpression;

/// Parse the tokens from the given range into an expression tree.
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
            assert(input.front.tok == Tok.LEFT_CURLY, input.front.s);
            input.popFront(); // eat the {
            auto formals = parseFormals(input);
            assert(formals, input.front.s);
            assert(input.front.tok == Tok.RIGHT_CURLY, input.front.s);
            input.popFront(); // eat the }
            assert(input.front.tok == Tok.COLON, input.front.s);
            input.popFront(); // eat the :
            auto body = parseExpression(input);
            assert(body, input.front.s);
            return new ExprLambda(body, t.s, formals);
        case Tok.COLON:
            input.popFront(); // eat the id
            input.popFront(); // eat the :
            auto body = parseExpression(input);
            assert(body, input.front.s);
            return new ExprLambda(body, t.s, null);
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
        assert(f.front.tok == Tok.RIGHT_CURLY, f.front.s);
        f.popFront(); // eat the }
        switch (f.front.tok) {
        case Tok.AT:
            input = f; // fast forward
            input.popFront(); // eat the @
            assert(input.front.tok == Tok.IDENTIFIER, input.front.s);
            const id = input.front.s;
            input.popFront(); // eat the id
            assert(input.front.tok == Tok.COLON, input.front.s);
            input.popFront(); // eat the :
            auto body = parseExpression(input);
            assert(body, input.front.s);
            return new ExprLambda(body, id, formals);
        case Tok.COLON:
            input = f; // fast forward
            input.popFront(); // eat the :
            auto body = parseExpression(input);
            assert(body, input.front.s);
            return new ExprLambda(body, null, formals);
        default:
            // assert(0, input.front.s);
            break;
        }
        goto default;
    case Tok.ASSERT:
        input.popFront(); // eat the assert
        auto expr = parseExpression(input);
        assert(expr, input.front.s);
        assert(input.front.tok == Tok.SEMICOLON, input.front.s);
        input.popFront(); // eat the ;
        return new ExprAssert(expr, parseExpression(input));
    case Tok.WITH:
        input.popFront(); // eat the with
        auto expr = parseExpression(input);
        assert(expr, input.front.s);
        assert(input.front.tok == Tok.SEMICOLON, input.front.s);
        input.popFront(); // eat the ;
        return new ExprWith(expr, parseExpression(input));
    case Tok.LET:
        auto let = input.save();
        let.popFront(); // eat the let
        if (let.front.tok == Tok.LEFT_CURLY)
            goto default;
        input = let; // fast forward
        auto binds = parseBinds(input);
        assert(binds, input.front.s);
        assert(0 == binds.dynamicAttrs.length); // dynamic attributes not allowed here
        assert(input.front.tok == Tok.IN, input.front.s);
        input.popFront(); // eat the in
        return new ExprLet(binds, parseExpression(input));
    default:
        return parseIf(input);
    }
}

private Expr parseIf(R)(ref R input) pure if (isTokenRange!R) {
    debug(PARSER) scope(exit) writeln("parseIf <- ");
    switch (input.front.tok) {
    case Tok.IF:
        input.popFront(); // eat the if
        auto cond = parseExpression(input);
        assert(cond, input.front.s);
        assert(input.front.tok == Tok.THEN, input.front.s);
        input.popFront(); // eat the then
        auto then = parseExpression(input);
        assert(then, input.front.s);
        assert(input.front.tok == Tok.ELSE, input.front.s);
        input.popFront(); // eat the else
        auto else_ = parseExpression(input);
        assert(else_, input.front.s);
        return new ExprIf(cond, then, else_);
    default:
        return parseOp(input);
    }
}

private Expr parseOp(R)(ref R input) pure if (isTokenRange!R) {
    debug(PARSER) writeln("parseOp ", input.front.tok, input.front.s);
    debug(PARSER) scope(exit) writeln("parseOp <- ");
    switch (input.front.tok) {
    case Tok.NOT:
        input.popFront(); // eat the !
        auto expr = parseOp(input);
        debug(PARSER) writeln("parseOp: parseOp returned ", format(expr));
        if (auto bin = cast(BinaryExpr) expr) {
            const assoc = associativity(Tok.NOT, bin.operator);
            assert(assoc != Associativity.NONE, "Non-associative");
            if (assoc == Associativity.LEFT) {
                // left-associative
                bin.left = new ExprOpNot(bin.left);
                return bin;
            }
        }
        // right-associative
        return new ExprOpNot(expr);
    case Tok.NEGATE:
        input.popFront(); // eat the -
        auto expr = parseOp(input);
        debug(PARSER) writeln("parseOp: parseOp returned ", format(expr));
        debug import std.stdio:write,writeln;
        import nix.printer : print;
        debug write(' ',Tok.NEGATE,' ');
        debug print(expr);
        if (auto bin = cast(BinaryExpr) expr) {
            const assoc = associativity(Tok.NEGATE, bin.operator);
            debug writeln(' ',bin.operator, " => ", assoc);
            assert(assoc != Associativity.NONE, "Non-associative");
            if (assoc == Associativity.LEFT) {
                // left-associative
                bin.left = new ExprBinaryOp(Tok.NEGATE, new ExprInt(0), bin.left);
                return bin;
            }
        }
        // right-associative
        return new ExprBinaryOp(Tok.NEGATE, new ExprInt(0), expr);
    default:
        auto left = parseApp(input);
        debug(PARSER) writeln("parseOp: parseApp returned ", format(left));
    case_op:
        if (!left || input.empty)
            return left;
        assert(left, input.front.s);
        auto op = input.front.tok;
        switch (op) {
        case Tok.HAS_ATTR:
            input.popFront(); // eat the ?
            auto ap = parseAttrPath(input);
            assert(ap, input.front.s);
            left = new ExprOpHasAttr(left, ap);
            goto case_op;
        case Tok.NEGATE:
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
            input.popFront(); // eat the op
            auto right = parseOp(input);
            debug(PARSER) writeln(__LINE__, " parseOp: parseOp returned ", format(right));
            debug import std.stdio:write,writeln;
            import nix.printer : print;
            debug print(left);
            debug write(' ',op,' ');
            debug print(right);
            if (auto bin = cast(BinaryExpr) right) {
                const assoc = associativity(op, bin.operator);
                debug writeln(' ',bin.operator, " => ", assoc);
                assert(assoc != Associativity.NONE, "Non-associative");
                if (assoc == Associativity.LEFT) {
                    // left-associative
                    bin.left = new ExprBinaryOp(op, left, bin.left);
                    return bin;
                }
            }
            // right-associative
            return new ExprBinaryOp(op, left, right);
        default:
            return left;
        }
    }
}

private void assertThrow(E=Error,T)(lazy T dg, in char[] msg = "Expected thrown "~E.stringof~", but got "~T.stringof) {
    try {
        dg();
    } catch(E) {
        return;
    }
    assert(false, msg);
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
    assertThrow(parse("2 == 3 == 4"));
    assertThrow(parse("2 != 3 != 4"));
}

private Expr parseApp(R)(ref R input) pure if (isTokenRange!R) {
    debug(PARSER) writeln("parseApp ", input.front.tok, input.front.s);
    auto select = parseSelect(input);
    debug(PARSER) writeln("parseApp: parseSelect returned ", format(select));
    if (!select)
        return null;
    while (true) {
        auto arg = parseSelect(input);
        if (!arg)
            break;
        select = new ExprBinaryOp(Tok.APP, select, arg);
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
    switch (input.front.tok) {
    case Tok.SELECT:
        assert(arg);
        input.popFront(); // eat the .
        auto attrpath = parseAttrPath(input);
        if (input.front.tok == Tok.OR_KW) {
            input.popFront(); // eat the or
            auto def = parseSelect(input);
            return new ExprSelect(arg, attrpath, def);
        } else {
            return new ExprSelect(arg, attrpath);
        }
    case Tok.OR_KW:
        assert(arg);
        input.popFront(); // eat the or
        /* Backwards compatibility: because Nixpkgs has a rarely used
            function named ‘or’, allow stuff like ‘map or [...]’. */
        return new ExprBinaryOp(Tok.APP, arg, new ExprVar("or"));
    default:
        return arg;
    }
}

private AttrName parseAttr(R)(ref R input) pure if (isTokenRange!R) {
    switch (input.front.tok) {
    case Tok.DOLLAR_CURLY:
        input.popFront(); // eat the ${
        auto expr = parseExpression(input);
        assert(input.front.tok == Tok.RIGHT_CURLY);
        input.popFront(); // eat the }
        return AttrName(expr);
    case Tok.STRING: // FIXME: could contain ${}
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

private void addAttr(ExprAttrs attrs, AttrPath ap, Expr e) pure {
    assert(ap.length > 0);
    foreach (i; ap[0 .. $ - 1]) {
        if (i.ident) {
            const j = i.ident in attrs.attrs;
            if (j) {
                assert(!j.inherited);
                attrs = cast(ExprAttrs) j.value;
                assert(attrs);
            } else {
                auto nested = new ExprAttrs();
                attrs.attrs[i.ident] = ExprAttrs.AttrDef(nested);
                attrs = nested;
            }
        } else {
            auto nested = new ExprAttrs();
            attrs.dynamicAttrs ~= ExprAttrs.DynamicAttrDef(i.expr, nested);
            attrs = nested;
        }
    }
    auto i = ap[$ - 1];
    if (i.ident) {
        const j = i.ident in attrs.attrs;
        if (j) {
            auto ae = cast(ExprAttrs) e;
            assert(ae);
            auto jAttrs = cast(ExprAttrs) j.value;
            assert(jAttrs);
            foreach (k, v; ae.attrs) {
                const j2 = k in jAttrs.attrs;
                assert(!j2); // Attr already defined in jAttrs, error.
                jAttrs.attrs[k] = v;
            }
        } else {
            // This attr path is not defined. Let's create it.
            attrs.attrs[i.ident] = ExprAttrs.AttrDef(e);
        }
    } else {
        attrs.dynamicAttrs ~= ExprAttrs.DynamicAttrDef(i.expr, e);
    }
}
