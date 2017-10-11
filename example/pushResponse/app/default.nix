{ mkDerivation, base, primitive, reflex-dom
, reflex-websocket-interface, reflex-websocket-interface-shared
, shared, stdenv
}:
mkDerivation {
  pname = "example";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base primitive reflex-dom reflex-websocket-interface
    reflex-websocket-interface-shared shared
  ];
  license = stdenv.lib.licenses.gpl3;
}
