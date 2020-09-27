let
  compiler = "ghc884";
  rev  = "b78e08e981a9ad31036fc6c6fb880c1315b4ebea";
  pkgs =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    }) {};

in pkgs.haskellPackages.callPackage ./achille.nix {}
