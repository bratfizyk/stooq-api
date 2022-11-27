{ mkDerivation, base, bytestring, Cabal, cassava, hashable, lens, stdenv, semialign,
  text, time, time-compat, utf8-string, vector, wreq }:
mkDerivation {
  pname = "stooq-api";
  version = "0.4.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  executableHaskellDepends = [
    base bytestring Cabal cassava hashable lens semialign
    text time time-compat utf8-string vector wreq
  ];
  license = "MIT";
  hydraPlatforms = stdenv.lib.platforms.none;
}
