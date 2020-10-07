with import ./nixpkgs.nix {};
let
  dmd' = dmd.overrideAttrs(oldAttrs: {
    prePatch = ''
      ls -l phobos/std
    '';# ++ oldAttrs.prePatch;
    patches = [ ./0001-Add-public-printFloat.patch ] ++ oldAttrs.patches;
  });
in mkShell {
  buildInputs = [
    dmd'
    dub
    ldc
  ];
}
