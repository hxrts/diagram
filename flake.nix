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
      packages.default = haskellPackages.callCabal2nix "diagram" ./. {
        isLibrary = false;
        isExecutable = true;
        buildTarget = "test"; # Points to the test component in the Cabal file
      };

      # Development shell
      devShell = pkgs.mkShell {
        buildInputs = with haskellPackages; [
          ghc                      # The Glasgow Haskell Compiler
          cabal-install            # Tool for building and managing Haskell projects
          hlint                    # Haskell linter
          haskell-language-server  # For IDE integration
          ormolu                   # Code formatter
          hspec                    # Test framework
        ];
      };

      # Test suite
      checks.defaultTest = haskellPackages.callCabal2nix "diagram-tests" ./. {
        isLibrary = false;
        isExecutable = true;
        buildTarget = "test";
      };
    });
}
