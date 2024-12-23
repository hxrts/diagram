{
  description = "Nix flake for diagram project";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ haskell-flake.flakeModule ];
      perSystem = { self', inputs', pkgs, lib, ... }: {
        haskellProjects.default = { };
        packages.default = self'.packages.diagram;
      };
    };
}
