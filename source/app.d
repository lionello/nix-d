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
        }
        catch (Throwable e) {
            writeln("Failed: ", filename);
            writeln(e);
        }
        // Compare with exp
        // const exp = readText(filename.replace(".nix", ".exp")).strip();
        // auto value2 = eval(parse(exp));
        // value2.forceValueDeep();
        // writeln(value2);
        // assert(value == value2);
        // writeln("Done.");
    }
}
