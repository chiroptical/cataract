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
      "yesod-auth-oauth2" =
        hself.callCabal2nix
        "yesod-auth-oauth2"
        sources.yesod-auth-oauth2
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
      pkgs.haskellPackages.stylish-haskell
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
      curl
    ];
    runAsRoot = let
      getFileName = fileName: with pkgs.lib; last (splitString "/" fileName);
      static = "${./static}";
      staticFileName = getFileName static;
      config = "${./config}";
      configFileName = getFileName config;
    in ''
      workDir=/build/pluto
      mkdir -p $workDir

      cp -r ${./static} $workDir
      mv $workDir/${staticFileName} $workDir/static

      cp -r ${./config} $workDir
      mv $workDir/${configFileName} $workDir/config
    '';
    config = {
      WorkingDir = "/build/pluto";
      Cmd = [
        "${exe}/bin/pluto"
      ];
    };
    created = "now";
  };
in {
  inherit shell;
  inherit myHaskellPackages;
  inherit docker;
  "pluto" = myHaskellPackages."pluto";
}
