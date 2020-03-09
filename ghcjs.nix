# XXX: NOT WORKING RIGHT NOW
let bootstrap = import <nixpkgs> {};

in

  with (builtins.fromJSON (builtins.readFile ./nixpkgs.ghcjs.json));

let
  jsaddle-src = pkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "1e39844";
    sha256 = "1qrjrjagmrrlcalys33636w5cb67db52i183masb7xd93wir8963";
  };
  jsaddle-dom-src = pkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle-dom";
    rev = "6ce23c5";
    sha256 = "1wpwf025czibkw6770c99zk7r30j6nh7jdzhzqbi2z824qyqzbnw";
  };
  ghcjs-dom-src = pkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-dom";
    rev = "b8e483a";
    sha256 = "06qlbbhjd0mlv5cymp5q0rb69a333l0fcif5zwa83h94dh25c1g7";
  };
  overrides = pkgs: with pkgs.haskell.lib; {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghc864 = pkgs.haskell.packages.ghc864.override {
          overrides = self: super: with pkgs.haskell.lib; {
            happy = dontCheck (super.callHackage "happy" "1.19.9" {});
            mkDerivation = args: super.mkDerivation (args // {
              enableLibraryProfiling = false;
              doCheck = false;
              doHaddock = false;
            });
          };
        };
        ghc865 = pkgs.haskell.packages.ghc865.override {
          overrides = self: super: with pkgs.haskell.lib; rec {
            jsaddle = self.callCabal2nix "jsaddle" "${jsaddle-src}/jsaddle" {};
            jsaddle-dom = self.callCabal2nix "jsaddle-dom" jsaddle-dom-src {};
            miso = self.callCabal2nix "miso" miso-src-filter {};
            miso-jsaddle = self.callCabal2nixWithOptions "miso" miso-src-filter "-fjsaddle" {};
            miso-examples-jsaddle = self.callCabal2nixWithOptions "miso-examples" miso-examples-src-filter "-fjsaddle" { miso = miso-jsaddle; };
            jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${jsaddle-src}/jsaddle-warp" {});
            ghcjs-dom-jsaddle = self.callCabal2nix "ghcjs-dom-jsaddle" "${ghcjs-dom-src}/ghcjs-dom-jsaddle" {};
            ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" "${ghcjs-dom-src}/ghcjs-dom-jsffi" {};
            ghcjs-dom = self.callCabal2nix "ghcjs-dom" "${ghcjs-dom-src}/ghcjs-dom" {};
            backend = super.callPackage ./backend {};
            masse-prelude = super.callPackage (pkgs.fetchFromGitHub {
              owner = "MasseR";
              repo = "masse-prelude";
              rev = "11662985f6865d73722d00ba22ea4fd1a338efbb";
              sha256 = "165mkmi1yl6pclvy3mixqsna4nmkdav23nabj3n3w3jz875x124f";
            }) {};
          };
        };
        ghcjs86 = pkgs.haskell.packages.ghcjs86.override {
          overrides = self: super: with pkgs.haskell.lib; {
            inherit (pkgs.haskell.packages.ghc865) hpack;
            common = super.callPackage ./common {};
            frontend = super.callPackage ./frontend {};
            api = super.callPackage ./. {};
            # Bump genvalidity. Breaking change between what we're using on the
            # ghc side and what's within the ghcjs pin
            genvalidity = self.callHackage "genvalidity" "0.8.0.0" {};
            genvalidity-text = self.callHackage "genvalidity-text" "0.6.0.0" {};
            genvalidity-containers = self.callHackage "genvalidity-containers" "0.6.0.0" {};
            jsaddle = self.callCabal2nix "jsaddle" "${jsaddle-src}/jsaddle" {};
            jsaddle-dom = self.callCabal2nix "jsaddle-dom" jsaddle-dom-src {};
            jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${jsaddle-src}/jsaddle-warp" {});
            ghcjs-dom-jsaddle = self.callCabal2nix "ghcjs-dom-jsaddle" "${ghcjs-dom-src}/ghcjs-dom-jsaddle" {};
            ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" "${ghcjs-dom-src}/ghcjs-dom-jsffi" {};
            ghcjs-dom = self.callCabal2nix "ghcjs-dom" "${ghcjs-dom-src}/ghcjs-dom" {};
            mkDerivation = args: super.mkDerivation (args // { doCheck = false; });
            doctest = null;
            miso = self.callCabal2nix "miso" (pkgs.fetchFromGitHub {
              owner = "dmjio";
              repo = "miso";
              inherit (builtins.fromJSON (builtins.readFile ./miso.json)) sha256 rev;
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
  build = pkgs.writeScriptBin "build" ''
    #!${pkgs.bash}/bin/bash
    cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build $@
  '';


in

rec {
  inherit (pkgs.haskell.packages.ghcjs86) api common frontend;
  miso-docs = pkgs.haskell.packages.ghcjs86.miso.doc;
  shell = pkgs.buildEnv {
    name = "shell";
    paths = [];
    buildInputs = with pkgs; [
      entr
      haskell.packages.ghc865.cabal-install
      nodejs
      build
      (haskell.packages.ghcjs86.ghcWithPackages (_: frontend.buildInputs ++ frontend.propagatedBuildInputs))
    ];
  };
}
