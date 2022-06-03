module nix.visitor;

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

template ConstVisitorT(T...) {
    static if (T.length > 1) {
    interface ConstVisitorT : ConstVisitorT!(T[0]), ConstVisitorT!(T[1..$]) {}
    } else {
    interface ConstVisitorT : VisitorT!(const T[0]), ConstVisitor {}
    }
}

abstract class Visitable {
    void accept(Visitor v) { const t = this; t.accept(v); }
    void accept(Visitor) const { assert(0, "No visit method found for "~this.classinfo.name); }
}

template Accept() {
    override void accept(Visitor v) {
        debug import std.stdio : writeln;
        debug scope(failure) writeln("while visiting ", this);
        auto visitorT = cast(VisitorT!(typeof(this))) v;
        return visitorT ? visitorT.visit(this) : super.accept(v);
    }
    override void accept(Visitor v) const {
        debug import std.stdio : writeln;
        debug scope(failure) writeln("while visiting ", this);
        auto visitorT = cast(VisitorT!(typeof(this))) v;
        return visitorT ? visitorT.visit(this) : super.accept(v);
    }
}

unittest {
    class Base : Visitable {
    }
    class Derived : Base {
        mixin Accept;
    }
    class Visitor : ConstVisitorT!Derived {
        bool visited;
        bool test(in Base b) {
            b.accept(this);
            return visited;
        }
        protected override void visit(in Derived d) {
            visited = true;
        }
    }
    assert(new Visitor().test(new Derived()));
}
