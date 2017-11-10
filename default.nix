haskellPackages: rec {
    reflex-websocket-interface = haskellPackages.callPackage ./reflex {inherit reflex-websocket-interface-shared;};
    reflex-websocket-interface-server = haskellPackages.callPackage ./server {inherit reflex-websocket-interface-shared;};
    reflex-websocket-interface-shared = haskellPackages.callPackage ./shared {};
}
