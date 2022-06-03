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
