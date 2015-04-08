{ mkDerivation, base, hsp, hsx2hs, reform, stdenv, text }:
mkDerivation {
  pname = "reform-hsp";
  version = "0.2.6";
  src = ./.;
  buildDepends = [ base hsp hsx2hs reform text ];
  homepage = "http://www.happstack.com/";
  description = "Add support for using HSP with Reform";
  license = stdenv.lib.licenses.bsd3;
}
