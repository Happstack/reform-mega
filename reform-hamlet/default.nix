{ mkDerivation, base, blaze-markup, reform, shakespeare, stdenv
, text
}:
mkDerivation {
  pname = "reform-hamlet";
  version = "0.0.5";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-markup reform shakespeare text
  ];
  homepage = "http://www.happstack.com/";
  description = "Add support for using Hamlet with Reform";
  license = stdenv.lib.licenses.bsd3;
}
