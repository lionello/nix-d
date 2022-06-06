module nix.path;

import std.exception : enforce;

alias Path = string;

immutable storeDir = "/nix/store";
immutable drvExtension = ".drv";

Path absPath(Path path) @safe {
    assert(path != "");
    if (path[0] == '~') {
        import std.path : expandTilde;
        path = expandTilde(path);
    }
    if (path[0] != '/') {
        import std.path : absolutePath;
        path = absolutePath(path);
    }
    return canonPath(path);
}

Path canonPath(in Path path) pure @safe {
    assert(path != "");
    if (path[0] != '/') {
        throw new Exception("not an absolute path: "~path);
    }
    // TODO: handle links
    import std.path : asNormalizedPath;
    import std.range : array;
    return asNormalizedPath(path).array;
}

bool isDerivation(in Path path) @safe {
    import std.algorithm : endsWith;
    return path.endsWith(drvExtension);
}

Path baseNameOf(in Path path) @safe {
    import std.path : baseName;
    return path == "/" ? "" : baseName(path);
}

unittest {
    assert(baseNameOf("") == "");
    assert(baseNameOf("/dir") == "dir");
    assert(baseNameOf("dir/foo") == "foo");
    assert(baseNameOf("/") == "");
    assert(baseNameOf("/dir/") == "dir");
}

/// Like std.path.dirName, but with slightly different behavior.
Path dirOf(in Path s) {
    import std.string : lastIndexOf;
    auto pos = s.lastIndexOf('/');
    if (pos == -1) return ".";
    return pos == 0 ? "/" : s[0..pos];
}

unittest {
    assert(dirOf("/") == "/");
    assert(dirOf("/dir/") == "/dir");
    assert(dirOf("/dir") == "/");
    assert(dirOf("/dir/..") == "/dir");
    assert(dirOf("/dir/../") == "/dir/..");
}

bool isStorePath(in string path) {
    return maybeParseStorePath(path) !is null;
}

Path maybeParseStorePath(in string path) {
    try {
        return parseStorePath(path);
    } catch(Exception) {
        return null;
    }
}

class BadStorePathException : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) pure {
        super(msg, file, line);
    }
}

Path parseStorePath(string s) {
    const p = canonPath(s);
    enforce!BadStorePathException(dirOf(p) == storeDir, "path '"~p~"' is not in the Nix store");
    return baseNameOf(p);
}

string printStorePath(in Path path) @safe pure {
    assert(path[0] != '/');
    return storeDir~"/"~path;
}
