module nix.main;

import std.stdio : writeln, write;

import nix.evaluator;
import nix.lexer : TokenRange;
import nix.parser : parseExpression;

void main(string[] args) {
    import std.file : readText;
    import std.string : replace;
    import std.string : strip;

    foreach (filename; args[1 .. $]) {
        writeln(filename);
        auto s = readText(filename);
        auto tr = TokenRange!string(s);
        scope (failure)
            writeln(filename, ", error on line ", tr.loc.line);
        auto eval = new Evaluator;
        parseExpression(tr).accept(eval);
        writeln(eval.value);
        // Compare with exp
        const exp = readText(filename.replace(".nix", ".exp")).strip();
        writeln(exp);
        assert(eval.value.toString() == exp);
        writeln("Done.");
    }
}
