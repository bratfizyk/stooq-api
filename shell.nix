{ pkgs ? import ./pkgs.nix {} }:

pkgs.stdenv.mkDerivation {
  name = "stooq-api-shell";
  buildInputs = with pkgs; [ (import ./release.nix) zlib ghc cabal-install haskell-language-server ];
}