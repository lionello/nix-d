with import <nixpkgs> {};
mkShell {
  buildInputs = [
    dmd
    rdmd
    dub
    #ldc
  ];
}
