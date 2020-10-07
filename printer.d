module nix.printer;

import std.stdio : writeln, write;

import nix.parser;

class Printer : Visitor {
    private static immutable string[Tok] ops;

    shared static this() {
        ops = [
            Tok.APP: " ",
            Tok.CONCAT: " ++ ",
            Tok.MUL: " * ",
            Tok.DIV: " / ",
            Tok.ADD: " + ",
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

    void writeAttrPath(AttrPath ap) {
        foreach (a; ap) {
            write(".", a.ident);
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

    void visit(in ExprBinaryOp e) {
        visit(e.left);
        write(ops[e.op]);
        visit(e.right);
    }

    void visit(in ExprInt e) {
        write(e.n);
    }

    void visit(in ExprFloat e) {
        write(e.f);
    }

    void visit(in ExprString e) {
        write(e.s);
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
            write(k, "=");
            visit(v.value);
            writeln(';');
        }
        write("}");
    }

    void visit(in ExprList e) {
        write("[ ");
        foreach (k; e.elems) {
            visit(k);
            write(' ');
        }
        write("]");
    }

    void visit(in ExprLambda e) {
        write(e.arg, "@");
        if (e.formals) {
            write("{");
            foreach (a; e.formals.elems)
                write(a.name, ",");
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
        writeln(";");
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
        writeln("assert ");
        visit(e.cond);
        writeln(';');
        visit(e.body);
    }
}
