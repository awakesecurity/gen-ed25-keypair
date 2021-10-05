let
  pkgs = import <nixpkgs> { };

in
pkgs.haskellPackages.callCabal2nix "gen-ed25-keypair" ./. { }
