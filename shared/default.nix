{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "reflex-websocket-interface-shared";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  license = stdenv.lib.licenses.unfree;
}
