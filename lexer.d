module nix.lexer;

public import std.range;
debug import std.stdio : writeln;
import std.range.primitives : isForwardRange, ElementType;
import std.ascii : isWhite, isDigit;

private enum EOF = -1;

/// Lexical tokens
enum Tok {
    ERROR,

    // Operators:
    SELECT, // non-assoc
    APP, // non-assoc
    NEGATE, // non-assoc
    HAS_ATTR, // non-assoc
    CONCAT, // right
    MUL, // left
    DIV, // left
    ADD, // left
    SUB, // left
    NOT, // left
    UPDATE, // right
    LT, // non-assoc
    LEQ, // non-assoc
    GT, // non-assoc
    GEQ, // non-assoc
    EQ, // non-assoc
    NEQ, // non-assoc
    AND, // left
    OR, // left
    IMPL, // right

    // Symbols:
    DOLLAR_CURLY, // ${
    COMMA,
    COLON,
    SEMICOLON,
    ASSIGN,
    LEFT_CURLY,
    RIGHT_CURLY,
    LEFT_PARENS,
    RIGHT_PARENS,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    WHITESPACE, // optional
    COMMENT, // #…
    MULTILINE, // /*…*/
    AT,
    ELLIPSIS, // ...
    STRING_OPEN, // "…
    STRING_CLOSE,  // …"
    IND_STRING_OPEN, // ''…
    IND_STRING_CLOSE, // …''

    // Literals:
    PATH, // …/…
    HPATH, // ~…
    SPATH, // <…>
    URI,
    STRING, // "…" (unparsed)
    IND_STRING, // ''…'' (unparsed)
    FLOAT,
    INT,
    IDENTIFIER,
    STR, // string content
    // IND_STR, // indent-string content

    // Keywords (identifiers):
    IF,
    THEN,
    ELSE,
    ASSERT,
    WITH,
    LET,
    IN,
    REC,
    INHERIT,
    OR_KW,
}

/// Associativity of operators
enum Associativity {
    NONE,
    LEFT,
    RIGHT,
}

private auto precedence(Tok tok) pure {
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
        import std.conv:to;
        assert(0, "no precedence for op "~to!string(tok));
    }
}

/// Returns the relative associativity of two given operators
Associativity associativity(Tok left, Tok right) pure {
    const lp = left.precedence;
    const rp = right.precedence;
    // debug writeln("%prec ", lp, " ", rp);
    if (lp.prec < rp.prec) {
        return Associativity.RIGHT;
    } else if (lp.prec > rp.prec) {
        return Associativity.LEFT;
    } else {
        return lp.assoc;
    }
}

unittest {
    assert(associativity(Tok.LEQ, Tok.LEQ) == Associativity.NONE);
    assert(associativity(Tok.MUL, Tok.ADD) == Associativity.LEFT);
    assert(associativity(Tok.ADD, Tok.MUL) == Associativity.RIGHT);
    assert(associativity(Tok.CONCAT, Tok.CONCAT) == Associativity.RIGHT);
}

/// Location in source file
struct Loc {
    uint line; //, col;
}

/// A lexed token
struct Token {
    Tok tok;
    union {
        string s;
        // Token[] t;
        // NixFloat f;
        // NixInt n;
    }

    Loc loc;
}

// https://github.com/NixOS/nix/blob/d048577909e383439c2549e849c5c2f2016c997e/src/libexpr/lexer.l#L91
private bool isIdChar(dchar d) pure {
    switch (d) {
    case 'a': .. case 'z':
    case 'A': .. case 'Z':
    case '0': .. case '9':
    case '_':
    case '\'':
    case '-':
            return true;
    default:
        return false;
    }
}

// https://github.com/NixOS/nix/blob/d048577909e383439c2549e849c5c2f2016c997e/src/libexpr/lexer.l#L97
private bool isUriSchemeChar(dchar d) pure {
    switch (d) {
    case 'a': .. case 'z':
    case 'A': .. case 'Z':
    case '0': .. case '9':
    case '+':
    case '-':
    case '.':
            return true;
    default:
        return false;
    }
}

// https://github.com/NixOS/nix/blob/d048577909e383439c2549e849c5c2f2016c997e/src/libexpr/lexer.l#L97
private bool isUriPathChar(dchar d) pure {
    switch (d) {
    case 'a': .. case 'z':
    case 'A': .. case 'Z':
    case '0': .. case '9':
    case '%':
    case '/':
    case '?':
    case ':':
    case '@':
    case '&':
    case '=':
    case '$':
    case ',':
    case '-':
    case '_':
    case '.':
    case '!':
    case '~':
    case '*':
    case '\'':
    case '+':
            return true;
    default:
        return false;
    }
}

// https://github.com/NixOS/nix/blob/d048577909e383439c2549e849c5c2f2016c997e/src/libexpr/lexer.l#L94
private bool isPathChar(dchar d) pure {
    switch (d) {
    case 'a': .. case 'z':
    case 'A': .. case 'Z':
    case '0': .. case '9':
    case '.':
    case '_':
    case '-':
    case '+':
            return true;
    default:
        return false;
    }
}

private Token[] parseDollar(R)(ref R input) pure if (isForwardRange!R) {
    assert(input.front == '$');
    input.popFront(); // eat the $
    if (input.front != '{') {
        return null;
    }
    input.popFront(); // eat the {
    Token[] tokens = [Token(Tok.DOLLAR_CURLY)];
    auto level = 1;
    while (true) {
        const t = popToken(input, true);
        tokens ~= t;
        switch (t.tok) {
        case Tok.DOLLAR_CURLY:
        case Tok.LEFT_CURLY:
            ++level;
            break;
        case Tok.RIGHT_CURLY:
            if (--level == 0)
                return tokens;
            break;
        case Tok.STRING_OPEN:
            tokens ~= parseString(input, false);
            break;
        case Tok.IND_STRING_OPEN:
            tokens ~= parseIndString(input, false);
            break;
        default:
            break;
        }
    }
}

unittest {
    auto r = `${a+{b="x${a}";}.b}`;
    assert([
        Token(Tok.DOLLAR_CURLY),
        Token(Tok.IDENTIFIER, "a"),
        Token(Tok.ADD),
        Token(Tok.LEFT_CURLY),
        Token(Tok.IDENTIFIER, "b"),
        Token(Tok.ASSIGN),
        Token(Tok.STRING_OPEN),
        Token(Tok.STR, "x"),
        Token(Tok.DOLLAR_CURLY),
        Token(Tok.IDENTIFIER, "a"),
        Token(Tok.RIGHT_CURLY),
        Token(Tok.STRING_CLOSE),
        Token(Tok.SEMICOLON),
        Token(Tok.RIGHT_CURLY),
        Token(Tok.SELECT),
        Token(Tok.IDENTIFIER, "b"),
        Token(Tok.RIGHT_CURLY)] == parseDollar(r));
    assert(r.empty, r);
}

private C unescapeChar(C)(C ch) @safe @nogc pure nothrow {
    switch (ch) {
    case 'n': return '\n';
    case 'r': return '\r';
    case 't': return '\t';
    default: return ch;
    }
}

/// Parse a string literal into separated tokens
Token[] parseString(R)(ref R input, bool popOpen = true) pure if (isForwardRange!R) {
    Token[] tokens;
    if (popOpen) {
        assert(input.front == '"', input);
        input.popFront(); // eat the "
        tokens ~= Token(Tok.STRING_OPEN);
    }
    assert(!input.empty);
    string str;
    void flush() {
        if (str) { tokens ~= Token(Tok.STR, str); str = null; }
    }
    while (true) {
        switch (input.front) {
        case '\\':
            input.popFront(); // eat the \
            str ~= unescapeChar(input.front);
            input.popFront(); // eat the escaped char
            break;
        case '"':
            flush();
            input.popFront(); // eat the "
            return tokens ~ Token(Tok.STRING_CLOSE);
        case '$':
            if (auto t = parseDollar(input)) {
                flush();
                tokens ~= t;
            } else {
                str ~= '$';
            }
            break;
        default:
            str ~= input.front;
            input.popFront();
            break;
        }
    }
}

unittest {
    auto r = `"y${a}$4\\\n"`;
    assert([
        Token(Tok.STRING_OPEN),
        Token(Tok.STR, "y"),
        Token(Tok.DOLLAR_CURLY),
        Token(Tok.IDENTIFIER, "a"),
        Token(Tok.RIGHT_CURLY),
        Token(Tok.STR, "$4\\\n"),
        Token(Tok.STRING_CLOSE)] == parseString(r));
    assert(r.empty, r);
}

/// Parse a indent-string literal into separated tokens
Token[] parseIndString(R)(ref R input, bool popOpen = true) pure if (isForwardRange!R) {
    Token[] tokens;
    if (popOpen) {
        assert(input.front == '\'');
        input.popFront(); // eat the 1st '
        assert(input.front == '\'');
        input.popFront(); // eat the 2nd '
        tokens ~= Token(Tok.IND_STRING_OPEN);
    }
    assert(!input.empty);
    string str;
    void flush() {
        if (str) { tokens ~= Token(Tok.STR, str); str = null; }
    }
    while (true) {
        switch (input.front) {
        case '\'':
            input.popFront(); // eat the 1st '
            if (input.front == '\'') {
                input.popFront(); // eat the 2nd '
                switch (input.empty ? EOF : input.front) {
                case '$':
                    str ~= '$';
                    break;
                case '\'':
                    str ~= "''"; // ''' => ''
                    break;
                case '\\':
                    input.popFront(); // eat the \
                    str ~= unescapeChar(input.front);
                    break;
                case EOF:
                default:
                    flush();
                    return tokens ~ Token(Tok.IND_STRING_CLOSE);
                }
                input.popFront(); // eat the escaped char
            } else str ~= input.front;
            break;
        case '$':
            if (auto t = parseDollar(input)) {
                flush();
                tokens ~= t;
            } else {
                str ~= '$';
            }
            break;
        default:
            str ~= input.front;
            input.popFront();
            break;
        }
    }
}

unittest {
    auto r = `''y${a}'''c''$''\n''`;
    assert([
        Token(Tok.IND_STRING_OPEN),
        Token(Tok.STR, "y"),
        Token(Tok.DOLLAR_CURLY),
        Token(Tok.IDENTIFIER, "a"),
        Token(Tok.RIGHT_CURLY),
        Token(Tok.STR, "''c$\n"),
        Token(Tok.IND_STRING_CLOSE)] == parseIndString(r));
    assert(r.empty, r);
}

private bool parseURI(R)(ref R input) pure if (isForwardRange!R) {
    auto uri = input.save;
    while (!uri.empty && isUriSchemeChar(uri.front))
        uri.popFront();
    if (!uri.empty && uri.front == ':') {
        uri.popFront(); // eat the :
        if (!uri.empty && isUriPathChar(uri.front)) {
            while (!uri.empty && isUriPathChar(uri.front))
                uri.popFront();
            input = uri; // fast forward
            return true;
        }
    }
    return false;
}

private bool parsePath(R)(ref R input, bool slash = false, bool spath = false) pure if (isForwardRange!R) {
    auto path = input.save;
    while (true) {
        while (!path.empty && isPathChar(path.front))
            path.popFront();
        if (path.empty || path.front != '/')
            break;
        path.popFront(); // eat the slash
        if (path.empty || !isPathChar(path.front))
            break;
        path.popFront(); // eat the path-char
        slash = true;
    }
    if (slash) {
        if (spath) {
            if (path.empty || path.front != '>')
                return false;
            path.popFront(); // eat the >
        }
        input = path; // fast forward
    }
    return slash;
}

private Tok popNextTok(R)(ref R input, bool explodeString) pure if (isForwardRange!R) {
    const ch = input.front;
    input.popFront();
    switch (ch) {
    case ',':
        return Tok.COMMA;
    case '@':
        return Tok.AT;
    case '.': // SELECT or ELLIPSIS or PATH
        if (parsePath(input))
            return Tok.PATH;
        if (!input.empty && input.front == '.') {
            input.popFront(); // eat the .
            assert(input.front == '.');
            input.popFront(); // eat the .
            return Tok.ELLIPSIS;
        }
        return Tok.SELECT;
    case '?':
        return Tok.HAS_ATTR;
    case '+': // ADD or CONCAT or PATH
        if (parsePath(input))
            return Tok.PATH;
        switch (input.empty ? EOF : input.front) {
        case '+':
            input.popFront(); // eat the +
            return Tok.CONCAT;
        default:
            return Tok.ADD;
        }
    case '*':
        return Tok.MUL;
    case '/': // DIV or UPDATE or MULTILINE or PATH
        switch (input.empty ? EOF : input.front) {
        case '/':
            input.popFront(); // eat the /
            return Tok.UPDATE;
        case '*':
            while (true) {
                input.popFront();
                if (input.front == '*') {
                    input.popFront(); // eat the *
                    if (input.front == '/') {
                        input.popFront(); // eat the /
                        return Tok.MULTILINE;
                    }
                }
            }
        default:
            if (!input.empty && isPathChar(input.front)) {
                if (parsePath(input, true))
                    return Tok.PATH;
            }
            return Tok.DIV;
        }
    case '-': // SUB or NEGATE or IMPL or PATH
        if (parsePath(input))
            return Tok.PATH;
        switch (input.empty ? EOF : input.front) {
        case '>':
            input.popFront(); // eat the >
            return Tok.IMPL;
        default:
            return Tok.NEGATE; // or Tok.SUB
        }
    case '!': // NEQ or NOT
        switch (input.empty ? EOF : input.front) {
        case '=':
            input.popFront(); // eat the =
            return Tok.NEQ;
        default:
            return Tok.NOT;
        }
    case '&':
        assert(input.front == '&');
        input.popFront(); // eat the &
        return Tok.AND;
    case '|':
        assert(input.front == '|');
        input.popFront(); // eat the |
        return Tok.OR;
    case ':':
        return Tok.COLON;
    case ';':
        return Tok.SEMICOLON;
    case '=': // ASSIGN or EQ
        switch (input.empty ? EOF : input.front) {
        case '=':
            input.popFront(); // eat the =
            return Tok.EQ;
        default:
            return Tok.ASSIGN;
        }
    case '{':
        return Tok.LEFT_CURLY;
    case '}':
        return Tok.RIGHT_CURLY;
    case '(':
        return Tok.LEFT_PARENS;
    case ')':
        return Tok.RIGHT_PARENS;
    case '[':
        return Tok.LEFT_BRACKET;
    case ']':
        return Tok.RIGHT_BRACKET;
    case '#':
        while (!input.empty && input.front != '\n' && input.front != '\r')
            input.popFront();
        return Tok.COMMENT;
    case '<': // LT or LEQ or SPATH
        switch (input.empty ? EOF : input.front) {
        case '=':
            input.popFront(); // eat the =
            return Tok.LEQ;
        default:
            if (!input.empty && isPathChar(input.front)) {
                if (parsePath(input, true, true))
                    return Tok.SPATH;
            }
            return Tok.LT;
        }
    case '>': // GT or GEQ
        switch (input.empty ? EOF : input.front) {
        case '=':
            input.popFront(); // eat the =
            return Tok.GEQ;
        default:
            return Tok.GT;
        }
    case '"':
        if (explodeString)
            return Tok.STRING_OPEN;
        parseString(input, false);
        return Tok.STRING;
    case '\'':
        assert(input.front == '\'');
        input.popFront(); // eat the 2nd '
        if (explodeString)
            return Tok.IND_STRING_OPEN;
        parseIndString(input, false);
        return Tok.IND_STRING;
    case '0': .. case '9': // INT or FLOAT or PATH
        if (parsePath(input))
            return Tok.PATH;
        while (!input.empty && isDigit(input.front))
            input.popFront();
        if (!input.empty && input.front == '.') {
            input.popFront(); // eat the .
            while (!input.empty && isDigit(input.front))
                input.popFront();
            if (!input.empty && (input.front == 'e' || input.front == 'E')) {
                input.popFront(); // eat the E
                if (input.front == '-' || input.front == '+')
                    input.popFront(); // eat the -
                while (!input.empty && isDigit(input.front))
                    input.popFront();
            }
            return Tok.FLOAT;
        }
        return Tok.INT;
    case ' ':
    case '\t':
    case '\n':
    case '\r':
        while (!input.empty && isWhite(input.front))
            input.popFront();
        return Tok.WHITESPACE;
    case 'a': .. case 'z':
    case 'A': .. case 'Z': // URI or IDENTIFIER or PATH
        if (parsePath(input))
            return Tok.PATH;
        if (parseURI(input))
            return Tok.URI;
        while (!input.empty && isIdChar(input.front))
            input.popFront();
        return Tok.IDENTIFIER;
    case '_': // IDENTIFIER or PATH
        if (parsePath(input))
            return Tok.PATH;
        while (!input.empty && isIdChar(input.front))
            input.popFront();
        return Tok.IDENTIFIER;
    case '~':
        assert(input.front == '/');
        assert(parsePath(input, true));
        return Tok.HPATH;
    case '$':
        if (input.front == '{') {
            input.popFront(); // eat the {
            return Tok.DOLLAR_CURLY;
        }
        goto default;
    default:
        assert(0, input.length > 55 ? input[0 .. 55] : input);
    }
}

unittest {
    const tokens = [
        // Simple:
        "." : Tok.SELECT, "-" : Tok.NEGATE, "?" : Tok.HAS_ATTR, "++" : Tok.CONCAT,
        "*" : Tok.MUL, "/" : Tok.DIV, "+" : Tok.ADD, "!" : Tok.NOT,
        "//" : Tok.UPDATE, "<" : Tok.LT, "<=" : Tok.LEQ, ">" : Tok.GT,
        ">=" : Tok.GEQ, "==" : Tok.EQ, "!=" : Tok.NEQ, "&&" : Tok.AND,
        "||" : Tok.OR, "->" : Tok.IMPL, ":" : Tok.COLON, ";" : Tok.SEMICOLON,
        "=" : Tok.ASSIGN, "{" : Tok.LEFT_CURLY, "}" : Tok.RIGHT_CURLY,
        "(" : Tok.LEFT_PARENS, ")" : Tok.RIGHT_PARENS, "[" : Tok.LEFT_BRACKET,
        "]" : Tok.RIGHT_BRACKET, " " : Tok.WHITESPACE, "#" : Tok.COMMENT,
        "..." : Tok.ELLIPSIS, "@" : Tok.AT, "," : Tok.COMMA,
        "${" : Tok.DOLLAR_CURLY,
        // Complex:
        " \n\r\t" : Tok.WHITESPACE, "# blah" : Tok.COMMENT,
        "/*\n*/" : Tok.MULTILINE, `""` : Tok.STRING, `" "` : Tok.STRING,
        `" \""` : Tok.STRING, "''\n''" : Tok.IND_STRING,
        "'' ''$ ''" : Tok.IND_STRING, "0" : Tok.INT, "12" : Tok.INT,
        "1." : Tok.FLOAT, "1.2" : Tok.FLOAT, "1.e6" : Tok.FLOAT,
        "11.E-61" : Tok.FLOAT, "abc" : Tok.IDENTIFIER, "abc'" : Tok.IDENTIFIER,
        "a-b_c'" : Tok.IDENTIFIER, "~/asdf" : Tok.HPATH, "~/asd/f" : Tok.HPATH,
        "http://a.com" : Tok.URI,
        "ssh+git://user@pw:a.com:32/b%23?a=b&c=d+e" : Tok.URI, "a/b" : Tok.PATH,
        "/b" : Tok.PATH, "./b/c" : Tok.PATH, "0/x" : Tok.PATH,
        "<nixpkgs>" : Tok.SPATH, "<n/p>" : Tok.SPATH,
    ];
    foreach (s, t; tokens) {
        assert(popNextTok(s, false) == t, s);
        assert(s.empty, s);
    }
}

private T[] before(T)(T[] all, T[] after) pure {
    return all[0 .. after.ptr - all.ptr];
}

unittest {
    const all = "all";
    static assert(before(all, all[1 .. $]) == all[0 .. 1]);
}

private Tok tokenizeIdent(in char[] id) pure nothrow {
    switch (id) {
    case "assert":
        return Tok.ASSERT;
    case "else":
        return Tok.ELSE;
    case "if":
        return Tok.IF;
    case "in":
        return Tok.IN;
    case "inherit":
        return Tok.INHERIT;
    case "let":
        return Tok.LET;
    case "or":
        return Tok.OR_KW;
    case "rec":
        return Tok.REC;
    case "then":
        return Tok.THEN;
    case "with":
        return Tok.WITH;
    default:
        return Tok.IDENTIFIER;
    }
}

/// Pop the next token from the given forward range
Token popToken(R)(ref R input, bool explodeString = false) pure if (isForwardRange!R) {
    const save = input.save;
    const tok = popNextTok(input, explodeString);
    string body;
    switch (tok) {
    case Tok.IDENTIFIER:
    case Tok.STRING:
    case Tok.IND_STRING:
    case Tok.STR:
    // case Tok.IND_STR:
    case Tok.INT:
    case Tok.FLOAT:
    case Tok.PATH:
    case Tok.SPATH:
    case Tok.HPATH:
    case Tok.URI:
    case Tok.WHITESPACE:
    case Tok.COMMENT:
    case Tok.MULTILINE:
        body = before(save, input);
        // debug writeln(tok, body);
        break;
    default:
        break;
    }
    return Token(tok == Tok.IDENTIFIER ? tokenizeIdent(body) : tok, body);
}

/// TokenRange is a forward range that returns valid Tokens and tracks line number.
struct TokenRange(R) if (isForwardRange!R) {
    private R input;

    Loc loc = Loc(1);
    Token front;

    this(in R input) {
        this.input = input;
        popFront();
    }

    this(ref return scope const TokenRange!R tr) {
        this.input = tr.input.save();
        this.loc = tr.loc;
        this.front = tr.front;
    }

    @property bool empty() pure const {
        switch (front.tok) {
        case Tok.ERROR:
        case Tok.WHITESPACE:
        case Tok.COMMENT:
        case Tok.MULTILINE:
            return true;
        default:
            return false;
        }
    }

    void popFront() pure {
        front.tok = Tok.ERROR;
        while (!input.empty) {
            front = popToken(input);
            if (!empty)
                break;
            foreach (c; front.s) {
                if (c == '\n')
                    ++loc.line;
            }
        }
        front.loc = loc;
    }

    auto save() {
        return typeof(this)(this);
    }
}

unittest {
    static assert(isForwardRange!(TokenRange!string));
    auto r = TokenRange!string(" {/*\n*/} #x");
    const s = r.save();
    assert(!r.empty);
    assert(r.front.tok == Tok.LEFT_CURLY);
    r.popFront();
    assert(r.front.tok == Tok.RIGHT_CURLY);
    r.popFront();
    assert(r.empty, r.front.s);
    assert(r.loc.line == 2);
    assert(!s.empty);
    assert(s.front.tok == Tok.LEFT_CURLY);
    assert(s.loc.line == 1);
}

/// Trait to test whether a type implements a forward range for `Token`s.
public enum bool isTokenRange(R) = isForwardRange!R && is(ElementType!R == Token);

unittest {
    static assert(isTokenRange!(TokenRange!string));
    static assert(!isTokenRange!string);
}
