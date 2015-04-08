with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, hsp, reform, stdenv, text }:
             mkDerivation {
               pname = "reform-hsp";
               version = "0.2.5";
               src = ./.;
               buildDepends = [ base hsp reform text ];
               homepage = "http://www.happstack.com/";
               description = "Add support for using HSP with Reform";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
