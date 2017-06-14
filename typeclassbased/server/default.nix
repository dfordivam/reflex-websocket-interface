{ mkDerivation, aeson, base, bytestring
, reflex-websocket-interface-shared, stdenv
}:
mkDerivation {
  pname = "reflex-websocket-interface-server";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring reflex-websocket-interface-shared
  ];
  license = stdenv.lib.licenses.bsd3;
}
