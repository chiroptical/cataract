{
  description = "cataract";

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
      cataract = pkgs.callPackage ./cataract.nix {};
      docker = pkgs.callPackage ./docker.nix {};
      twitch-cli = pkgs.callPackage ./nix/twitch-cli.nix {};
    in {
      devShell = import ./shell.nix {
        inherit pkgs;
        inherit twitch-cli;
      };
      defaultPackage = cataract;
      packages = flake-utils.lib.flattenTree {
        inherit cataract;
        inherit docker;
      };
    });
}
