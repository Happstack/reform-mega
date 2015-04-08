with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, blaze-html, blaze-markup, reform, stdenv
             , text
             }:
             mkDerivation {
               pname = "reform-blaze";
               version = "0.2.3";
               src = ./.;
               buildDepends = [ base blaze-html blaze-markup reform text ];
               homepage = "http://www.happstack.com/";
               description = "Add support for using blaze-html with Reform";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
