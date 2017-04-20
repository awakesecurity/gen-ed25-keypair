{ ghc }:
let
  config = import ../config.nix;
  pkgs = import <nixpkgs> { inherit config; };
in with pkgs; pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "gen-ed25-keypair-stack-shell";
  buildInputs = [
    zlib cabal-install
  ];
}
