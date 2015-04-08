{ mkDerivation, base, bytestring, happstack-server, mtl, random
, reform, stdenv, text, utf8-string
}:
mkDerivation {
  pname = "reform-happstack";
  version = "0.2.5";
  src = ./.;
  buildDepends = [
    base bytestring happstack-server mtl random reform text utf8-string
  ];
  homepage = "http://www.happstack.com/";
  description = "Happstack support for reform";
  license = stdenv.lib.licenses.bsd3;
}
