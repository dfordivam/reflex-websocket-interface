{ nixpkgs ? import <nixpkgs> {}
, reflex-platform ? import ~/repos/reflex/reflex-platform {}
, ghc ? reflex-platform.ghc }:

let

  inherit (nixpkgs) pkgs;

  reflex-websocket-interface = ghc.callPackage        ../../../reflex {inherit reflex-websocket-interface-shared;};
  reflex-websocket-interface-shared = ghc.callPackage ../../../shared {};
  drv = ghc.callPackage ./default.nix {
    inherit reflex-websocket-interface-shared reflex-websocket-interface;
    shared = ghc.callPackage ../shared {inherit reflex-websocket-interface-shared;};
    };

in

  if pkgs.lib.inNixShell then drv.env else drv
