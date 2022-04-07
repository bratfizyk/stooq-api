{ mkDerivation, aeson, base, bytestring, Cabal, hashable, lens, stdenv, semialign,
  text, time, time-compat, utf8-string, vector, wreq }:
mkDerivation {
  pname = "stooq-api";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  executableHaskellDepends = [
    aeson base bytestring Cabal hashable lens semialign text time time-compat utf8-string vector wreq
  ];
  license = "MIT";
  hydraPlatforms = stdenv.lib.platforms.none;
}
