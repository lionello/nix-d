module nix.main;

import std.stdio : writeln, write;

import nix.evaluator : eval, forceValueDeep;
import nix.lexer : TokenRange;
import nix.parser : parse;

void main(string[] args) {
    import std.file : readText;
    import std.string : replace;
    import std.string : strip;

    foreach (filename; args[1 .. $]) {
        writeln(filename);
        auto s = readText(filename);
        auto tr = TokenRange!string(s);
        scope (failure)
            writeln(filename, ", error on line ", tr.front.loc.line);
        const value = eval(parse(tr)).forceValueDeep;
        writeln(value);
        // Compare with exp
        const exp = readText(filename.replace(".nix", ".exp")).strip();
        writeln(exp);
        assert(value.toString() == exp);
        writeln("Done.");
    }
}
