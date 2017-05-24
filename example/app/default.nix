{ mkDerivation, base, reflex-dom, reflex-websocket-interface
, reflex-websocket-interface-shared, stdenv
}:
mkDerivation {
  pname = "example";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base reflex-dom reflex-websocket-interface
    reflex-websocket-interface-shared
  ];
  license = stdenv.lib.licenses.gpl3;
}
