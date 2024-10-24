let
  rev = "75a35fd777948666c6175c73e08d0f0bc1a7974e";

  sha256 = "04cbj56i5qnaa93fam5k4lpz6im5bh5zl1811z78wga3iz3xcfca";

  pkgs =
    import
      (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
        inherit sha256;
      })
      { config = { }; };

in
pkgs.haskellPackages.callCabal2nix "gen-ed25-keypair" ./. { }
