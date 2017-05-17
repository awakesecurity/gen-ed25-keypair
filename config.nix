{ allowUnfree = true;
  packageOverrides = pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: {
        optparse-generic =
          haskellPackagesNew.callPackage ./nix/optparse-generic.nix { };

        gen-ed25-keypair = haskellPackagesNew.callPackage ./default.nix { };
      };
    };
  };
}
