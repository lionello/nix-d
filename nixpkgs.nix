args:
let
  nixpkgs = builtins.fetchTarball {
    name = "nixos-unstable-small";
    url = "https://github.com/NixOS/nixpkgs-channels/archive/3b00e78e63b6539369821660245cd579f6723b58.tar.gz";
    sha256 = "1gb3zyj18lxixmxzkivgr2mn787ndkf2c6vd88ks4h6781nlpplv";
  };
in import nixpkgs args
