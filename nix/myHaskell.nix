{pkgs, ...}: let
  yesod-auth-oauth2 = pkgs.callPackage ./yesod-auth-oauth2.nix {};
in
pkgs.haskell.packages.ghc8107.extend (final: prev: {
  "yesod-auth-oauth2" =
    final.callCabal2nix
      "yesod-auth-oauth2"
      yesod-auth-oauth2
      {};
})
