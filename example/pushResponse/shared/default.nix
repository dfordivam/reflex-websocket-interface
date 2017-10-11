{ mkDerivation, aeson, base, reflex-websocket-interface-shared
, stdenv, text
}:
mkDerivation {
  pname = "shared";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base reflex-websocket-interface-shared text
  ];
  license = stdenv.lib.licenses.gpl3;
}
