{ mkDerivation, aeson, base, binary, binary-instances, blaze-html
, blaze-markup, bytestring, containers, data-default, directory
, filepath, frontmatter, Glob, mtl, optparse-applicative, pandoc
, pandoc-types, process, stdenv, tasty, tasty-hunit, text, time
, yaml
}:
mkDerivation {
  pname = "achille";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary binary-instances blaze-html blaze-markup
    bytestring data-default directory filepath frontmatter Glob
    optparse-applicative pandoc pandoc-types process text time yaml
  ];
  testHaskellDepends = [
    base bytestring containers directory filepath Glob mtl tasty
    tasty-hunit text time
  ];
  description = "A library for building static site generators";
  license = stdenv.lib.licenses.mit;
}
