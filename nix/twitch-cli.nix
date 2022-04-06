{pkgs, ...}:
pkgs.stdenv.mkDerivation {
  name = "twitch-cli";
  src = pkgs.fetchurl {
    url = "https://github.com/twitchdev/twitch-cli/releases/download/v1.1.5/twitch-cli_1.1.5_Linux_x86_64.tar.gz";
    sha256 = "sha256-uUT/T4dWDFNAl0vWlSRu8fIC1qJPkQK6QjBAun517Lo";
  };
  # Found this here: https://github.com/NixOS/nixpkgs/blob/93a139dac3cadd2949c62966e54f54f4ec1f74fa/pkgs/tools/misc/ent/default.nix#L12-L14
  setSourceRoot = "sourceRoot=`pwd`";
  phases = [
    "unpackPhase"
    "installPhase"
  ];
  installPhase = ''
    mkdir -p $out/bin
    cp twitch $out/bin/twitch
  '';
}
