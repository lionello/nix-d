module nix.assoc;

import nix.lexer : Tok;

/// Associativity of operators
enum Associativity {
    NONE,
    LEFT,
    RIGHT,
}

private auto precedence(Tok tok) pure @safe {
    struct Precedence {
        int prec;
        Associativity assoc;
    }
    switch (tok) {
    case Tok.SELECT:
        return Precedence(13, Associativity.NONE);
    case Tok.APP:
        return Precedence(12, Associativity.NONE);
    case Tok.NEGATE:
        return Precedence(11, Associativity.NONE);
    case Tok.HAS_ATTR:
        return Precedence(10, Associativity.NONE);
    case Tok.CONCAT:
        return Precedence(9, Associativity.RIGHT);
    case Tok.MUL:
    case Tok.DIV:
        return Precedence(8, Associativity.LEFT);
    case Tok.ADD:
    case Tok.SUB:
        return Precedence(7, Associativity.LEFT);
    case Tok.NOT:
        return Precedence(6, Associativity.LEFT);
    case Tok.UPDATE:
        return Precedence(5, Associativity.RIGHT);
    case Tok.LT:
    case Tok.LEQ:
    case Tok.GT:
    case Tok.GEQ:
        return Precedence(4, Associativity.NONE);
    case Tok.EQ:
    case Tok.NEQ:
        return Precedence(3, Associativity.NONE);
    case Tok.AND:
    case Tok.OR:
        return Precedence(2, Associativity.LEFT);
    case Tok.IMPL:
        return Precedence(1, Associativity.RIGHT);
    default:
        import std.conv: text;
        assert(0, "no precedence for op "~text(tok));
    }
}

/// Returns the relative associativity of two given operators
Associativity associativity(Tok left, Tok right) pure @safe {
    const lp = left.precedence;
    const rp = right.precedence;
    if (lp.prec < rp.prec) {
        return Associativity.RIGHT;
    } else if (lp.prec > rp.prec) {
        return Associativity.LEFT;
    } else {
        return lp.assoc;
    }
}

@safe unittest {
    assert(associativity(Tok.LEQ, Tok.LEQ) == Associativity.NONE);
    assert(associativity(Tok.MUL, Tok.ADD) == Associativity.LEFT);
    assert(associativity(Tok.SUB, Tok.SUB) == Associativity.LEFT);
    assert(associativity(Tok.ADD, Tok.MUL) == Associativity.RIGHT);
    assert(associativity(Tok.CONCAT, Tok.CONCAT) == Associativity.RIGHT);
}
