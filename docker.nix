{pkgs, ...}: let
  cataract = import ./cataract.nix pkgs;
  exe = with pkgs.haskell.lib; dontCheck (justStaticExecutables cataract);
in
  pkgs.dockerTools.buildImage {
    name = "cataract";
    contents = with pkgs; [
      bash
      coreutils
      curl
      cacert
    ];
    runAsRoot = let
      getFileName = fileName: with pkgs.lib; last (splitString "/" fileName);
      static = "${./static}";
      staticFileName = getFileName static;
      config = "${./config}";
      configFileName = getFileName config;
    in ''
      workDir=/build/cataract
      mkdir -p $workDir

      cp -r ${./static} $workDir
      mv $workDir/${staticFileName} $workDir/static

      cp -r ${./config} $workDir
      mv $workDir/${configFileName} $workDir/config
    '';
    config = {
      WorkingDir = "/build/cataract";
      Cmd = [
        "${exe}/bin/cataract"
      ];
    };
    created = "now";
  }
