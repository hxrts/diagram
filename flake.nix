{
  description = "Haskell development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in {
      devShell = pkgs.mkShell {
        packages = with pkgs.haskellPackages; [
          ghc                    # The Glasgow Haskell Compiler
          cabal-install          # Tool for building and managing Haskell projects
          hlint                  # Haskell linter
          haskell-language-server # For IDE integration
        ];
      };
    });
}