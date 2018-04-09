{ allowUnfree = true;
  packageOverrides = pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: {
        optparse-applicative =
          haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { };

        optparse-generic =
          haskellPackagesNew.callPackage ./nix/optparse-generic.nix { };

        gen-ed25-keypair = haskellPackagesNew.callPackage ./default.nix { };
      };
    };
  };
}
