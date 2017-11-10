haskellPackages: {
    reflex-websocket-interface = haskellPackages.callPackage ./reflex {};
    reflex-websocket-interface-server = haskellPackages.callPackage ./server {};
    reflex-websocket-interface-shared = haskellPackages.callPackage ./shared {};
}
