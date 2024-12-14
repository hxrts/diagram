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
          hlint
          haskell-language-server  # for IDE integration
          ormolu                   # code formatter
          hspec                    # test framework
        ];
      };

      # Test suite
      checks.tests = pkgs.runCommand "tests" {
        buildInputs = with haskellPackages; [
          ghc
          cabal-install
          hspec
          hspec-discover
        ];
        src = ./.;
      } ''
        export HOME=$(mktemp -d)
        mkdir -p $HOME/.config/cabal

        # Copy source files
        cp -r $src/* .

        # Run cabal test
        cabal update
        cabal build
        cabal test

        # Write success marker to the output directory
        mkdir -p $out
        echo "Tests passed successfully!" > $out/result.txt
      '';
    });
}
