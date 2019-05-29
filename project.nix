nixpkgs:

let

  inherit (nixpkgs.lib) mapAttrs mapAttrsToList escapeShellArg optionalString concatStringsSep concatMapStringsSep;

in

{ haskellPackages
, lib
, packages
, overrides ? _ : _ : {}
, tools ? []
}:

let

  filtered = src: lib.sourceByRegex src [
    ".*.cabal"
    "src.*"
    "LICENSE"
    "migrations.*"
    "resources.*"
    "config.*"
  ];
  overrides' = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
    (self: super: mapAttrs (name: path: self.callCabal2nix name (filtered path) {}) packages)
    overrides
  ];
  haskellPackages' = haskellPackages.extend overrides';
  packages' = mapAttrs (name: _: haskellPackages'."${name}") packages;
  mkShell = name: pkg:
  let
    n =  "${name}-shell";
    deps = with haskellPackages'; [
      nixpkgs.binutils-unwrapped
      ghcid
      cabal-install
      hasktags
      (haskellPackages'.ghcWithHoogle (pkgs: pkg.buildInputs ++ pkg.propagatedBuildInputs))
    ];
  in
  {
    name = "${n}";
    value = nixpkgs.buildEnv {
      name = "${n}";
      paths = tools;
      buildInputs = tools ++ [deps];
    };
  };
  shells = nixpkgs.lib.listToAttrs (mapAttrsToList mkShell packages');

in

packages' // shells
