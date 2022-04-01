{pkgs, ...}:
pkgs.mkShell {
  inputsFrom = [
    (import ./pluto.nix pkgs).env
  ];
  buildInputs = with pkgs; [
    haskell-language-server
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.stylish-haskell
    haskellPackages.hlint
    haskellPackages.hpack
    haskellPackages.retrie
    niv
    alejandra

    # animatorium
    nodejs
    esbuild
    nodePackages.npm
  ];
  withHoogle = true;
}
