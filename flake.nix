{
  description = "Homing Pigeon";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowBroken = true;
      };
      pluto = pkgs.callPackage ./pluto.nix {};
      docker = pkgs.callPackage ./docker.nix {};
      twitch-cli = pkgs.callPackage ./nix/twitch-cli.nix {};
    in {
      devShell = import ./shell.nix {
        inherit pkgs;
        inherit twitch-cli;
      };
      defaultPackage = pluto;
      packages = flake-utils.lib.flattenTree {
        inherit pluto;
        inherit docker;
      };
    });
}
