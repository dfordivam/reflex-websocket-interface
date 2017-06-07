{ mkDerivation, aeson, base, bytestring, containers, dependent-map
, dependent-sum, mtl, prim-uniq, primitive, reflex, reflex-dom
, reflex-websocket-interface-shared, stdenv, text
}:
mkDerivation {
  pname = "reflex-websocket-interface";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers dependent-map dependent-sum mtl
    prim-uniq primitive reflex reflex-dom
    reflex-websocket-interface-shared text
  ];
  license = stdenv.lib.licenses.unfree;
}
