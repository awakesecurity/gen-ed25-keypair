{ mkDerivation, base, bytestring, cryptonite, memory, mtl
, optparse-applicative, optparse-generic, stdenv, system-filepath
, tasty, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "gen-ed25-keypair";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cryptonite memory mtl optparse-applicative
    optparse-generic text
  ];
  executableHaskellDepends = [
    base bytestring cryptonite mtl optparse-generic system-filepath
    text
  ];
  testHaskellDepends = [
    base bytestring cryptonite memory optparse-generic tasty
    tasty-hunit tasty-quickcheck text
  ];
  homepage = "https://github.com/awakenetworks/gen-ed25-keypair#readme";
  description = "Ed25519 keypair generator, message signing and verification tools";
  license = stdenv.lib.licenses.asl20;
}
