{ mkDerivation, aeson, base, stdenv }:
mkDerivation {
  pname = "reflex-websocket-interface-shared";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base ];
  license = stdenv.lib.licenses.unfree;
}
