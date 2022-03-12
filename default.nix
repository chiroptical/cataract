{compiler ? "ghc8107"}: let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [./.gitignore];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "pluto" =
        hself.callCabal2nix
        "pluto"
        (gitignore ./.)
        {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."pluto"
    ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.fourmolu
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.hpack
      pkgs.niv
      pkgs.alejandra
    ];
    withHoogle = true;
  };

  exe = with pkgs.haskell.lib;
    dontCheck (justStaticExecutables (myHaskellPackages."pluto"));

  docker = pkgs.dockerTools.buildImage {
    name = "pluto";
    contents = with pkgs; [
      bash
      coreutils
    ];
    runAsRoot = let
      getFileName = fileName: with pkgs.lib; last (splitString "/" fileName);
      static = "${./static}";
      staticFileName = getFileName static;
      config = "${./config}";
      configFileName = getFileName config;
    in ''
      mkdir /data

      cp -r ${./static} /data
      mv /data/${staticFileName} /data/static

      cp -r ${./config} /data
      mv /data/${configFileName} /data/config
    '';
    config = {
      WorkingDir = "/data";
      Cmd = [
        "${exe}/bin/pluto"
      ];
    };
  };
in {
  inherit shell;
  inherit myHaskellPackages;
  inherit docker;
  "pluto" = myHaskellPackages."pluto";
}
