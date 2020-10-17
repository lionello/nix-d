module nix.printer;

static import std.stdio;

import nix.parser;
private import std.conv : to;
private import std.range : isOutputRange;

enum isStringWriter(W) = is(typeof(W.init.write("")));

private struct StringWriter {
    char[] b;
    void write(string s) { b ~= s; }
}

char[] format(in Expr expr) {
    StringWriter sw;
    print(expr, &sw);
    return sw.b;
}

void print(in Expr expr) {
    print(expr, std.stdio.stdout);
}

void println(in Expr expr) {
    println(expr, std.stdio.stdout);
}

void print(W)(in Expr expr, auto ref W writer) if (isStringWriter!W) {
    expr.accept(new Printer!W(writer));
}

void println(W)(in Expr expr, auto ref W writer) if (isStringWriter!W) {
    print!W(expr, writer);
    writer.write("\n");
}

unittest {
    StringWriter sw;
    println(new ExprString("x"), &sw);
    assert(sw.b == "\"x\"\n");
}

// void print(R)(in Expr expr, ref R writer) if (isOutputRange!(R,char)) {
//     struct Writer {
//         void write(string) @safe {}
//     }
//     expr.accept(new Printer!Writer(Writer.init));
// }

string escapeString(string s) @safe pure nothrow {
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

private class Printer(W) if (isStringWriter!W) : Visitor {
    private static immutable string[Tok] ops;

    shared static this() {
        ops = [
            Tok.APP: " ",
            Tok.CONCAT: " ++ ",
            Tok.MUL: " * ",
            Tok.DIV: " / ",
            Tok.ADD: " + ",
            Tok.NEGATE: "-",
            Tok.SUB: " - ",
            Tok.UPDATE: " // ",
            Tok.LT: " < ",
            Tok.LEQ: " <= ",
            Tok.GT: " > ",
            Tok.GEQ: " >= ",
            Tok.EQ: " == ",
            Tok.NEQ: " != ",
            Tok.AND: " && ",
            Tok.OR: " || ",
            Tok.IMPL: " -> ",
        ];
    }

    private W writer;

    void write(string s) {
        writer.write(s);
    }

    this(ref W writer) {
        this.writer = writer;
    }

    void writeAttrPath(in AttrPath ap) {
        foreach (a; ap) {
            write(".");
            write(a.ident);
            visit(a.expr);
        }
    }

    void visit(in Expr e) {
        if (e)
            e.accept(this);
    }

    void visit(in ExprOpNot e) {
        write("!");
        visit(e.expr);
    }

    void visit(in ExprNop e) {
        visit(e.expr);
    }

    void visit(in ExprBinaryOp e) {
        write("(");
        visit(e.left);
        write(ops[e.op]);
        visit(e.right);
        write(")");
    }

    void visit(in ExprInt e) {
        write(to!string(e.n));
    }

    void visit(in ExprFloat e) {
        write(to!string(e.f));
    }

    void visit(in ExprString e) {
        write(escapeString(e.s));
    }

    void visit(in ExprPath e) {
        write(e.p);
    }

    void visit(in ExprVar e) {
        write(e.name);
    }

    void visit(in ExprSelect e) {
        visit(e.left);
        writeAttrPath(e.ap);
    }

    void visit(in ExprOpHasAttr e) {
        visit(e.left);
        write(" ? ");
        writeAttrPath(e.ap);
    }

    void visit(in ExprAttrs e) {
        write("{");
        foreach (k, v; e.attrs) {
            write(k);
            write("=");
            visit(v.value);
            write(";\n");
        }
        write("}");
    }

    void visit(in ExprList e) {
        write("[ ");
        foreach (k; e.elems) {
            visit(k);
            write(" ");
        }
        write("]");
    }

    void visit(in ExprLambda e) {
        write(e.arg);
        write("@");
        if (e.formals) {
            write("{");
            foreach (a; e.formals.elems)
                write(a.name);
                write(",");
            if (e.formals.ellipsis)
                write("...");
            write("}");
        }
        write(":");
        visit(e.body);
    }

    void visit(in ExprLet e) {
        write("let ");
        visit(e.attrs);
        write(" in ");
        visit(e.body);
    }

    void visit(in ExprWith e) {
        write("with ");
        visit(e.attrs);
        write(";\n");
        visit(e.body);
    }

    void visit(in ExprIf e) {
        write("if ");
        visit(e.cond);
        write(" then ");
        visit(e.then);
        write(" else ");
        visit(e.else_);
    }

    void visit(in ExprAssert e) {
        write("assert ");
        visit(e.cond);
        write(";\n");
        visit(e.body);
    }
}
