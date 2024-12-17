{
  description = "Haskell development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

 outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      haskellPackages = pkgs.haskellPackages;
    in {
      # Main executable package
      packages.default = haskellPackages.developPackage {
        name = "diagram";
        root = ./.;
      };

        # Development shell
        devShells.default = pkgs.mkShell {
          packages = with haskellPackages; [
            ghc
            cabal-install
          ];
        };
      });
}
