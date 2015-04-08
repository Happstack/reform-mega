{ mkDerivation, base, hsp, hsx2hs, reform, stdenv, text }:
mkDerivation {
  pname = "reform-hsp";
  version = "0.2.5";
  src = ./.;
  buildDepends = [ base hsp reform text hsx2hs ];
  homepage = "http://www.happstack.com/";
  description = "Add support for using HSP with Reform";
  license = stdenv.lib.licenses.bsd3;
}
