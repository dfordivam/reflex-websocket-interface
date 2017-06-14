{ mkDerivation, aeson, base, bytestring
, reflex-websocket-interface-server
, reflex-websocket-interface-shared, shared, stdenv, text
, websockets
}:
mkDerivation {
  pname = "server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring reflex-websocket-interface-server
    reflex-websocket-interface-shared shared text websockets
  ];
  license = stdenv.lib.licenses.gpl3;
}
