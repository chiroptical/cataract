{pkgs, ...}:

pkgs.dockerTools.buildImage {
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
}
