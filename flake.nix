{
  description = "haskell development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs"; 
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.defaultApp {
    inputs = { inherit nixpkgs; };
    defaultPackage = nixpkgs.lib.mkShell {
      packages = pkgs: with pkgs.haskellPackages; [
        ghc              # The Glasgow Haskell Compiler
        cabal-install    # Tool for building and managing Haskell projects
        hlint            # Haskell linter
        haskell-language-server # For IDE integration
      ];
    };
  };
}