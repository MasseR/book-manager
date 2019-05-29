{ lib, nixpkgs, haskellPackages }:

let
  miso = nixpkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "630e823dd40a434b73124e12b229a79d9fefb01d";
    sha256 = "046gdp3ah2lsipfcy89rh20mn08xbhcgrj549v8zzy69j33xjm2l";
  };
  servant = nixpkgs.fetchFromGitHub {
    owner = "haskell-servant";
    repo = "servant";
    rev = "v0.15";
    sha256 = "0n9xn2f61mprnvn9838zbl4dv2ynnl0kxxrcpf5c0igdrks8pqws";
  };
  miso-jsaddle = super: if haskellPackages.ghc.isGhcjs or false then (super.callPackage (miso + "/miso-ghcjs.nix") {}) else (super.callPackage (miso + "/miso-ghc-jsaddle.nix") {});


in

(import ./project.nix nixpkgs) {
  inherit lib haskellPackages;
  packages = {
    api = ./api;
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };
  overrides = self: super: with nixpkgs.haskell.lib; {
    miso = miso-jsaddle super;
    generic-lens = dontCheck super.generic-lens;
    # servant = dontCheck (super.callCabal2nix "servant" (servant + "/servant") {});
    # servant-client-ghcjs = dontCheck (super.callCabal2nix "servant" (servant + "/servant-client-ghcjs") {});
    # servant-server = dontCheck (super.callCabal2nix "servant" (servant + "/servant-server") {});

  };
  tools = with haskellPackages; [
    ghcid
    hasktags
  ];
}
