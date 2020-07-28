# Copied from https://nixos.org/nixos/nix-pills/callpackage-design-pattern.html
let
  nixpkgs = import <nixpkgs> {};
  allPkgs = nixpkgs // pkgs;
  callPackage = path: overrides:
    let f = import path;
    in f ((builtins.intersectAttrs (builtins.functionArgs f) allPkgs) // overrides);
  pkgs = with nixpkgs; {
    fms = callPackage ./fms { };
    esmf = callPackage ./esmf { };
    nceplibs = callPackage ./nceplibs { };
    fv3 = callPackage ./fv3 { };
  };
in pkgs