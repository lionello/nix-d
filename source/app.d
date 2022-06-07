module nix.main;

import std.stdio : writeln, write;
import std.exception : enforce;

import nix.evaluator;
import nix.lexer : TokenRange;
import nix.parser : parse;
import nix.printer;

immutable WHITE = "\033[97m";
immutable RESET = "\033[0m";
immutable RED = "\033[91m";
immutable GREEN = "\033[92m";
immutable YELLOW = "\033[93m";
immutable BLUE = "\033[94m";

void main(string[] args) {
    import std.file : readText, exists;
    import std.string : replace;
    import std.string : strip;

    foreach (filename; args[1 .. $]) {
        if (exists(filename.replace(".nix", ".exp-disabled"))) continue;
        writeln(WHITE, "\nFile: ", filename, RESET);
        auto s = readText(filename);
        // print(ast);
        // writeln("=");
        try {
            const ast = parseAndBind(s);
            auto value = eval(ast);
            value.forceValueDeep();
            if (value.type == Type.Lambda) {
                // FIXME: read args from .flags file
                auto lib = eval(parseAndBind("import(./lib.nix)"));
                auto attrs = Value(["lib": &lib, "xyzzy": Value("xyzzy!", null).dup]);
                value = callFunction(value, attrs);
                value.forceValueDeep();
                value = *value.attrs["result"];
            }
            writeln(BLUE, "Result: ", value.toString(10), RESET);
            try {
                // Compare with .exp, if it exists
                const exp = readText(filename.replace(".nix", ".exp")).strip();
                auto value2 = eval(parseAndBind(exp));
                value2.forceValueDeep();
                enforce(value == value2, "Expected: "~value2.toString~"\nGot: "~value.toString~"\n");
            }
            catch (Exception e) {
                // writeln("Failed: ", e.message);
            }
        }
        catch (Exception e) {
            writeln(RED, "Failed: ", e, RESET);
            // break;
        }
        // writeln("Done.");
    }
}
