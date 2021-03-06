module nix.main;

import std.stdio : writeln, write;

import nix.evaluator;
import nix.lexer : TokenRange;
import nix.parser : parse;
import nix.printer;

void main(string[] args) {
    import std.file : readText, exists;
    import std.string : replace;
    import std.string : strip;

    foreach (filename; args[1 .. $]) {
        if (exists(filename.replace(".nix", ".exp-disabled"))) continue;
        writeln(filename);
        auto s = readText(filename);
        const ast = parse(s);
        // print(ast);
        // writeln("=");
        try {
            auto value = eval(ast);
            value.forceValueDeep();
            if (value.type == Type.Lambda) {
                // FIXME: read args from .flags file
                auto lib = eval(parse("import(./lib.nix)"));
                auto b = Value(["lib": &lib, "xyzzy": Value("xyzzy!", null).dup]);
                value = callFunction(value, b, Loc());
                value.forceValueDeep();
                value = *value.attrs["result"];
            }
            writeln(value.toString(10));
            try {
                // Compare with .exp, if it exists
                const exp = readText(filename.replace(".nix", ".exp")).strip();
                auto value2 = eval(parse(exp));
                value2.forceValueDeep();
                assert(value == value2, "Expected: "~value2.toString);
            }
            catch (Exception e) {
                // writeln("Failed: ", e.message);
            }
        }
        catch (Exception e) {
            writeln("Failed: ", e);
        }
        // writeln("Done.");
    }
}
