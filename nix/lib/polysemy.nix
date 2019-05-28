{ mkDerivation, base, criterion, fetchgit, free, freer-simple
, hspec, inspection-testing, mtl, random, stdenv, syb
, template-haskell, transformers
}:
mkDerivation {
  pname = "polysemy";
  version = "0.2.1.0";
  src = fetchgit {
    url = "https://github.com/isovector/polysemy";
    sha256 = "05pwxnyv3w0rhn2g3f1l2x08x69737sr2nlvg3mcbg0h66fja86x";
    rev = "b6f16e762bac1b4e68de5e93ac96230284d04017";
  };
  libraryHaskellDepends = [
    base mtl random syb template-haskell transformers
  ];
  testHaskellDepends = [
    base hspec inspection-testing mtl random syb template-haskell
    transformers
  ];
  benchmarkHaskellDepends = [
    base criterion free freer-simple mtl random syb template-haskell
    transformers
  ];
  homepage = "https://github.com/isovector/polysemy#readme";
  description = "Higher-order, low-boilerplate, zero-cost free monads";
  license = stdenv.lib.licenses.bsd3;
}
