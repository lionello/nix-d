module nix.main;

import std.stdio : writeln, write;

import nix.evaluator;
import nix.lexer : TokenRange;
import nix.parser : parse;
import nix.printer;

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
        const ast = parse(tr);
        print(ast);
        write("=");
        auto value = eval(ast);
        value = value.forceValueDeep;
        if (value.type == Type.Lambda) {
            // FIXME: read from .flags file
            auto lib = eval(parse("import(./lib.nix)"));
            Bindings b = ["lib": lib, "xyzzy": Value("xyzzy!", null)];
            value = callFunction(value, Value(b), Loc()).forceValueDeep;
            value = value.attrs["result"];
        }
        // Compare with exp
        const exp = readText(filename.replace(".nix", ".exp")).strip();
        const value2 = eval(parse(exp)).forceValueDeep;
        writeln(value);
        writeln(value2);
        assert(value == value2);
        writeln("Done.");
    }
}
