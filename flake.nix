{
  description = "MIPS";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
        packageName = "mips";
        package = haskellPackages.callCabal2nix packageName ./. { };
      in
      {
        packages.default = package;
        packages.${packageName} = package;
        
        devShells.default = (pkgs.haskell.lib.addBuildTools package [
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
        ]).env;
      });
}
