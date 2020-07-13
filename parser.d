module nix.parser;

debug import std.stdio : writeln;
import std.conv : to;

public import nix.lexer;

alias NixInt = long;
alias NixFloat = double;
alias Ident = string;

struct AttrName {
    Ident ident;
    Expr expr;
    this(Ident i) pure {
        assert(i);
        this.ident = i;
    }

    this(Expr e) pure {
        assert(e);
        this.expr = e;
    }
}

alias AttrPath = AttrName[];

interface Visitor {
    void visit(ExprOpNot);
    void visit(ExprBinaryOp);
    void visit(ExprInt);
    void visit(ExprFloat);
    void visit(ExprString);
    void visit(ExprPath);
    void visit(ExprVar);
    void visit(ExprSelect);
    void visit(ExprOpHasAttr);
    void visit(ExprAttrs);
    void visit(ExprList);
    void visit(ExprLambda);
    void visit(ExprLet);
    void visit(ExprWith);
    void visit(ExprIf);
    void visit(ExprAssert);
}

abstract class Expr {
    abstract void accept(Visitor v);
}

class ExprOpNot : Expr {
    Expr expr;
    this(Expr expr) pure {
        assert(expr);
        this.expr = expr;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

abstract class BinaryExpr : Expr {
    Expr left;
    @property abstract Tok operator() pure;
}

class ExprBinaryOp : BinaryExpr {
    Tok op;
    Expr right;
    this(Tok op, Expr left, Expr right) pure {
        assert(left);
        assert(right);
        this.op = op;
        this.left = left;
        this.right = right;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
    override Tok operator() pure { return op; }
}

class ExprSelect : BinaryExpr {
    AttrPath ap;
    Expr def;
    this(Expr left, AttrPath ap, Expr def = null) pure {
        assert(left);
        assert(ap);
        this.left = left;
        this.ap = ap;
        this.def = def;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
    override Tok operator() pure { return Tok.SELECT; }
}

class ExprOpHasAttr : BinaryExpr {
    AttrPath ap;
    this(Expr left, AttrPath ap) pure {
        assert(left);
        assert(ap);
        this.left = left;
        this.ap = ap;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
    override Tok operator() pure { return Tok.HAS_ATTR; }
}

abstract class ValueExpr : Expr {
}

class ExprInt : ValueExpr {
    NixInt n;
    this(NixInt n) pure {
        this.n = n;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

class ExprFloat : ValueExpr {
    NixFloat f;
    this(NixFloat f) pure {
        this.f = f;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

class ExprString : ValueExpr {
    string s;
    //context c;
    this(string s) pure {
        this.s = s;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

class ExprPath : ValueExpr {
    string p;
    this(string p) pure {
        this.p = p;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

class ExprVar : Expr {
    Ident name;
    // int level;
    //uint displ; // displacement
    this(Ident name) pure {
        assert(name);
        this.name = name;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

struct AttrDef {
    Expr value;
    bool inherited;
    //uint displ; // index into env
}

struct DynamicAttrDef {
    Expr name;
    Expr value;
}

class ExprAttrs : Expr {
    AttrDef[Ident] attrs;
    DynamicAttrDef[] dynamicAttrs;
    bool recursive;
    override void accept(Visitor v) {
        v.visit(this);
    }
}

class ExprList : Expr {
    Expr[] elems;
    this(Expr[] elems) pure {
        this.elems = elems;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

struct Formal {
    Ident name;
    Expr def;
}

class Formals {
    Formal[] elems;
    bool ellipsis;
}

class ExprLambda : Expr {
    Ident arg;
    Formals formals;
    Expr body;
    this(Ident arg, Formals formals, Expr body) pure {
        assert(body);
        this.arg = arg;
        this.formals = formals;
        this.body = body;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

class ExprLet : Expr {
    ExprAttrs attrs;
    Expr body;
    this(ExprAttrs attrs, Expr body) pure {
        assert(attrs);
        assert(body);
        this.attrs = attrs;
        this.body = body;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

class ExprWith : Expr {
    Expr attrs, body;
    // uint prevWith;
    this(Expr attrs, Expr body) pure {
        assert(attrs);
        assert(body);
        this.attrs = attrs;
        this.body = body;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

class ExprIf : Expr {
    Expr cond, then, else_;
    this(Expr cond, Expr then, Expr else_) pure {
        assert(cond);
        assert(then);
        assert(else_);
        this.cond = cond;
        this.then = then;
        this.else_ = else_;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

class ExprAssert : Expr {
    Expr cond, body;
    this(Expr cond, Expr body) pure {
        assert(cond);
        assert(body);
        this.cond = cond;
        this.body = body;
    }

    override void accept(Visitor v) {
        v.visit(this);
    }
}

ExprList parseList(R)(ref TokenRange!R input) pure {
    Expr[] elems;
    while (input.front.tok != Tok.RIGHT_BRACKET) {
        elems ~= parseSelect(input);
    }
    return new ExprList(elems);
}

AttrPath parseAttrs(R)(ref TokenRange!R input) pure {
    AttrPath ap;
    while (true) {
        auto an = parseAttr(input);
        if (an.ident == "" && an.expr is null)
            break;
        ap ~= an;
    }
    return ap;
}

ExprAttrs parseBinds(R)(ref TokenRange!R input) pure {
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
                    binds.attrs[ap.ident] = AttrDef(new ExprSelect(expr, [
                                ap
                            ]), true);
                }
            } else {
                foreach (ap; parseAttrs(input)) {
                    assert(ap.ident);
                    assert(ap.ident !in binds.attrs);
                    binds.attrs[ap.ident] = AttrDef(new ExprVar(ap.ident), true);
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

Expr parseSimple(R)(ref TokenRange!R input) pure {
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
        // case Tok.true_:
        // case Tok.false_:
        // case Tok.NULL:
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
        input.popFront();
        auto e = parseExpression(input);
        debug writeln("parseSimple: parseExpression returned ", e);
        assert(e, input.front.s);
        assert(input.front.tok == Tok.RIGHT_PARENS, input.front.s);
        input.popFront(); // eat the )
        return e;
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
        input.popFront();
        // TODO: make absolute
        return new ExprPath(t.s);
    case Tok.HPATH:
        input.popFront();
        const home = "/todo";
        return new ExprPath(home ~ t.s[1 .. $]);
    case Tok.SPATH:
        input.popFront();
        auto findNixPath = new ExprBinaryOp(Tok.APP, new ExprVar("__findFile"), new ExprVar("__nixPath"));
        return new ExprBinaryOp(Tok.APP, findNixPath, new ExprString(t.s));
    case Tok.STRING:
        input.popFront();
        // TODO: unescape/explode
        return new ExprString(t.s);
    case Tok.IND_STRING:
        input.popFront();
        // TODO: unescape/explode/strip
        return new ExprString(t.s);
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
    assert(cast(ExprInt) parse("(2)"));
    assert(cast(ExprFloat) parse("2."));
    assert(cast(ExprString) parse(`"str"`));
    assert(cast(ExprString) parse(`''\n str''`));
    assert(cast(ExprString) parse("http://a.com"));
    assert(cast(ExprPath) parse("/a"));
    assert(cast(ExprPath) parse("./a"));
    assert(cast(ExprPath) parse("~/a"));
    assert(cast(ExprBinaryOp) parse("<a>"));
    assert(cast(ExprVar) parse("libcap/* bla*/"));
}

Formals parseFormals(R)(ref TokenRange!R input) pure {
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

Expr parseExpression(R)(ref TokenRange!R input) pure {
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
            return new ExprLambda(t.s, formals, body);
        case Tok.COLON:
            input.popFront(); // eat the id
            input.popFront(); // eat the :
            auto body = parseExpression(input);
            assert(body, input.front.s);
            return new ExprLambda(t.s, null, body);
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
            return new ExprLambda(id, formals, body);
        case Tok.COLON:
            input = f; // fast forward
            input.popFront(); // eat the :
            auto body = parseExpression(input);
            assert(body, input.front.s);
            return new ExprLambda(null, formals, body);
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

Expr parseIf(R)(ref TokenRange!R input) pure {
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

Expr parseOp(R)(ref TokenRange!R input) pure {
    debug writeln("parseOp ", input.front.s);
    switch (input.front.tok) {
    case Tok.NOT:
        input.popFront(); // eat the !
        auto expr = parseOp(input);
        if (auto bin = cast(BinaryExpr) expr) {
            const assoc = associativity(Tok.NOT, bin.operator);
            assert(assoc != Associativity.NONE, "Non-associative");
            assert(assoc == Associativity.RIGHT, "TODO");
        }
        return new ExprOpNot(expr);
    case Tok.NEGATE:
        input.popFront(); // eat the -
        auto expr = parseOp(input);
        if (auto bin = cast(BinaryExpr) expr) {
            const assoc = associativity(Tok.NEGATE, bin.operator);
            assert(assoc != Associativity.NONE, "Non-associative");
            assert(assoc == Associativity.RIGHT, "TODO");
        }
        return new ExprBinaryOp(Tok.SUB, new ExprInt(0), expr);
    default:
        auto left = parseApp(input);
        debug writeln("parseOp: parseApp returned ", left);
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
        case Tok.CONCAT:
        case Tok.MUL:
        case Tok.DIV:
        case Tok.ADD:
        case Tok.SUB:
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
            if (auto bin = cast(BinaryExpr) right) {
                const assoc = associativity(op, bin.operator);
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

unittest {
    auto parse(string s) {
        auto tr = TokenRange!string(s);
        auto e = parseOp(tr);
        assert(tr.empty);
        return e;
    }

    assert(cast(ExprBinaryOp) parse("a + b"));
    assert(cast(ExprBinaryOp) parse("a + b - c"));
    assert(cast(ExprOpHasAttr) parse("a ? b"));
    assert(cast(ExprBinaryOp) parse("a ? b || c"));
    assert(cast(ExprBinaryOp) parse("a ? b || c ? d"));
    with(cast(ExprBinaryOp) parse("2 * 3 + 4")) { assert(op == Tok.ADD); }
    with(cast(ExprBinaryOp) parse("2 + 3 * 4")) { assert(op == Tok.ADD); }
    with(cast(ExprBinaryOp) parse("2 * 3 + 4 * 4")) { assert(op == Tok.ADD); }
    with(cast(ExprBinaryOp) parse("2 * 3 * 4 + 4")) { assert(op == Tok.ADD); }
    // assert(parse("2 == 3 == 4"));
    // assert(parse("2 != 3 != 4"));
}

Expr parseApp(R)(ref TokenRange!R input) pure {
    debug writeln("parseApp ", input.front.s);
    auto select = parseSelect(input);
    debug writeln("parseApp: parseSelect returned ", select);
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

Expr parseSelect(R)(ref TokenRange!R input) pure {
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

AttrName parseAttr(R)(ref TokenRange!R input) pure {
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

AttrPath parseAttrPath(R)(ref TokenRange!R input) pure {
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
                attrs.attrs[i.ident] = AttrDef(nested);
                attrs = nested;
            }
        } else {
            auto nested = new ExprAttrs();
            attrs.dynamicAttrs ~= DynamicAttrDef(i.expr, nested);
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
            attrs.attrs[i.ident] = AttrDef(e);
        }
    } else {
        attrs.dynamicAttrs ~= DynamicAttrDef(i.expr, e);
    }
}
