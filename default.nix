{ lib, nixpkgs, haskellPackages }:

let
  miso = nixpkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "630e823dd40a434b73124e12b229a79d9fefb01d";
    sha256 = "046gdp3ah2lsipfcy89rh20mn08xbhcgrj549v8zzy69j33xjm2l";
  };
  miso-jsaddle = super: if haskellPackages.ghc.isGhcjs or false then (super.callPackage (miso + "/miso-ghcjs.nix") {}) else (super.callPackage (miso + "/miso-ghc-jsaddle.nix") {});

  dontCheck = nixpkgs.haskell.lib.dontCheck;

in

(import ./project.nix nixpkgs) {
  inherit lib haskellPackages;
  packages = {
    api = ./api;
    backend = ./backend;
    frontend = ./frontend;
  };
  overrides = self: super: {
    miso = miso-jsaddle super;
  };
  tools = with haskellPackages; [
    ghcid
    hasktags
  ];
}
