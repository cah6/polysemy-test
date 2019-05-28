{ mkDerivation, base, fetchgit, ghc, ghc-tcplugins-extra, hspec
, inspection-testing, polysemy, should-not-typecheck, stdenv, syb
, transformers
}:
mkDerivation {
  pname = "polysemy-plugin";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/isovector/polysemy";
    sha256 = "05pwxnyv3w0rhn2g3f1l2x08x69737sr2nlvg3mcbg0h66fja86x";
    rev = "b6f16e762bac1b4e68de5e93ac96230284d04017";
  };
  postUnpack = "sourceRoot+=/polysemy-plugin; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base ghc ghc-tcplugins-extra polysemy syb transformers
  ];
  testHaskellDepends = [
    base ghc ghc-tcplugins-extra hspec inspection-testing polysemy
    should-not-typecheck syb transformers
  ];
  homepage = "https://github.com/isovector/polysemy#readme";
  description = "Disambiguate obvious uses of effects";
  license = stdenv.lib.licenses.bsd3;
}
