{
  pkgs,
  twitch-cli,
  ...
}:
pkgs.mkShell {
  inputsFrom = [
    (import ./cataract.nix pkgs).env
  ];
  buildInputs = with pkgs; [
    haskell-language-server
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.hpack
    haskellPackages.retrie
    haskellPackages.fourmolu
    niv
    alejandra
    twitch-cli

    # animatorium
    nodejs
    esbuild
    nodePackages.npm
  ];
  withHoogle = true;
}
