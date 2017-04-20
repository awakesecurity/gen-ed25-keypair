let config = import ./config.nix;
in
{ pkgs ? import <nixpkgs> { inherit config; } }:
let
  darwinPkgs = import <nixpkgs> { inherit config; system = "x86_64-darwin"; };
  linuxPkgs  = import <nixpkgs> { inherit config; system = "x86_64-linux" ; };
  pkgs       = import <nixpkgs> { inherit config; };

in
  { gen-ed25-keypair-linux  =  linuxPkgs.haskellPackages.gen-ed25-keypair;
    gen-ed25-keypair-darwin = darwinPkgs.haskellPackages.gen-ed25-keypair;
    gen-ed25-keypair        =       pkgs.haskellPackages.gen-ed25-keypair;
  }
