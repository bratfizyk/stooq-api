{ mkDerivation, aeson, base, bytestring, Cabal, hashable, lens, stdenv, semialign, stringsearch,
  text, time, time-compat, utf8-string, vector, wreq }:
mkDerivation {
  pname = "stooq-api";
  version = "0.3.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  executableHaskellDepends = [
    aeson base bytestring Cabal hashable lens semialign stringsearch
    text time time-compat utf8-string vector wreq
  ];
  license = "MIT";
  hydraPlatforms = stdenv.lib.platforms.none;
}
