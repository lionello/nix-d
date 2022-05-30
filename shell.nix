with import <nixpkgs> {};
mkShell {
  buildInputs = [
    #dmd
    dtools
    dub
    ldc
  ];
}
