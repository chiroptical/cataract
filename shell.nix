{pkgs, myHaskellPackages, ...}:

myHaskellPackages.shellFor {
  inputsFrom = [
    (import ./pluto.nix pkgs).env
  ];
  buildInputs = [
    myHaskellPackages.haskell-language-server
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.stylish-haskell
    pkgs.haskellPackages.hlint
    pkgs.haskellPackages.hpack
    pkgs.haskellPackages.retrie
    pkgs.niv
    pkgs.alejandra
  ];
  withHoogle = true;
}
