{ nixpkgs ? import <nixpkgs> {} }:

let

  jsaddle = nixpkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "34fe7d61b3f387b81aa748294ac8d993243f53b4";
    sha256 = "0qdh5qdk23vcp1yp910zgw2hs4zpbx9ig25xgaax0iwj2m1ifh5x";
  };
  ghcjs-dom = nixpkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-dom";
    rev = "b8e483adef0cea66d081c1a014e87c6f99eb29fc";
    sha256 = "06qlbbhjd0mlv5cymp5q0rb69a333l0fcif5zwa83h94dh25c1g7";
  };
  jsaddle-dom = nixpkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle-dom";
    rev = "dd1cc363e824e888ae29d61ac54d0e226d81fcdf";
    sha256 = "1lxgrjik3ldri54hmbbxn62sx46hrgnppmggdrhsaj4kzk8ysgj7";
  };
  config = {
    packageOverrides = pkgs: with pkgs.haskell.lib;  with pkgs.lib; {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ghccustom = pkgs.haskell.packages.ghc843.override {
            overrides = self: super: {
              jsaddle-warp = dontCheck (super.callPackage (jsaddle + "/jsaddle-warp") {});
              jsaddle-dom = super.callPackage jsaddle-dom {};
              # jsaddle-warp = super.callPackage ./jsaddle-warp-ghcjs.nix {};
              jsaddle = dontCheck (super.callPackage (jsaddle + "/jsaddle") {});
            } // import ghcjs-dom super;
          };
          ghcjscustom = pkgs.haskell.packages.ghcjs84.override {
            overrides = self: super: {
              doctest = null;
              comonad = dontCheck (super.comonad);
              classy-prelude = dontCheck (super.classy-prelude);
              unliftio = dontCheck (super.unliftio);
              semigroupoids = dontCheck (super.semigroupoids);
              lens = dontCheck (super.lens);
              directory-tree = dontCheck (super.directory-tree);
              http-types = dontCheck (super.http-types);
              tasty-quickcheck = dontCheck (super.tasty-quickcheck);
              scientific = dontCheck (super.scientific);
              servant = dontCheck (super.servant);
              jsaddle-warp = super.callPackage ./jsaddle-warp-ghcjs.nix {};
              jsaddle = dontCheck (super.callPackage (jsaddle + "/jsaddle") {});
              jsaddle-dom = super.callPackage jsaddle-dom {};
              cryptonite = dontCheck (super.cryptonite);
              ghc = overrideDerivation (super.ghc.override {
                ghcjsSrc = pkgs.fetchgit {
                  url = "https://github.com/ghcjs/ghcjs.git";
                  rev = "dc190b1bb2453cfa484124e9f335ee3cad1492f7";
                  sha256 = "0dh52gj0f3700zfyrhisy44b6y9p1bsawwrmd5pllpdyw21zd9lw";
                  fetchSubmodules = true;
                };
              }) (drv: { patches = (drv.patches or []) ++ [ ./ghcjs.patch ]; });
            } // import ghcjs-dom super;
          };
        };
      };
    };
  };
  pinnedVersion = nixpkgs.lib.importJSON ./nixpkgs-version.json;
  pinnedPkgs = import (nixpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (pinnedVersion) rev sha256;
  }) { inherit config; };
  ghc = pinnedPkgs.callPackage ./default.nix { nixpkgs = pinnedPkgs; haskellPackages = pinnedPkgs.haskell.packages.ghccustom; };
  # ghcjs = pinnedPkgs.callPackage ./default.nix { nixpkgs = pinnedPkgs; haskellPackages = pinnedPkgs.haskell.packages.ghcjscustom; };
  ghcjs = import ./default.nix { lib = pinnedPkgs.lib; nixpkgs = pinnedPkgs; haskellPackages = pinnedPkgs.haskell.packages.ghcjscustom; };
  mkDeps = deps: pkgs.buildEnv {
    name = "deps";
    paths = [ deps ];
  };
  inherit (pinnedPkgs) pkgs;

  in

{
  inherit ghc ghcjs;
  deps = {
    backend = mkDeps (pkgs.haskell.packages.ghccustom.ghcWithPackages (_: ghc.backend.buildInputs ++ ghc.backend.propagatedBuildInputs));
    frontend = mkDeps (pkgs.haskell.packages.ghcjscustom.ghcWithPackages (_: ghcjs.frontend.buildInputs ++ ghcjs.frontend.propagatedBuildInputs));
    frontendDev = mkDeps (pkgs.haskell.packages.ghccustom.ghcWithPackages (_: ghc.frontend.buildInputs ++ ghc.frontend.propagatedBuildInputs));
  };
}
