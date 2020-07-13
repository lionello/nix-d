with import ./nixpkgs.nix {};
mkShell {
  buildInputs = [
    dmd
    dub
    ldc
  ];
}
