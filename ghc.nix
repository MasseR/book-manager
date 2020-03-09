with (builtins.fromJSON (builtins.readFile ./nixpkgs.ghc.json));


let
  overrides = pkgs: with pkgs.haskell.lib; {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghc865 = pkgs.haskell.packages.ghc865.override {
          overrides = self: super: with pkgs.haskell.lib; rec {
            backend = super.callPackage ./backend {};
            api = super.callPackage ./api {};
            masse-prelude = super.callPackage (pkgs.fetchFromGitHub {
              owner = "MasseR";
              repo = "masse-prelude";
              rev = "6e3958ab011269acd17edbbe6be9d9c3c1633d43";
              sha256 = "1jspcr573iqw39m22ab37fn4wvjvbnh651zibmlx5qna90v5ccn9";
            }) {};
          };
        };
      };
    };
  };
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }) { config.packageOverrides = overrides; config.allowUnfree = true; };

in

rec {
  inherit (pkgs.haskell.packages.ghc865) api backend;
  shell = pkgs.buildEnv {
    name = "shell";
    paths = [];
    buildInputs = with pkgs; [
      entr
      ghcid
      haskell.packages.ghc865.cabal-install
      haskell.packages.ghc865.hasktags
      stylish-haskell
      hlint
      binutils
      (haskell.packages.ghc865.ghcWithHoogle (_: backend.buildInputs ++ backend.propagatedBuildInputs ++ api.buildInputs ++ api.propagatedBuildInputs))
    ];
  };
}
