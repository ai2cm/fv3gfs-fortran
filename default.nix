# Copied from https://nixos.org/nixos/nix-pills/callpackage-design-pattern.html
let
  vcmpkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "vcm-packages";
    url = "git@github.com:VulcanClimateModeling/packages.git";
    ref = "master";
    rev = "127f9a5d294103d541c9e1d829bea8e1b4252220";
  });
  # use local version of FMS
  fms = vcmpkgs.callPackage ./nix/fms { };
  fv3 = vcmpkgs.callPackage ./nix/fv3 { fms = fms;};
in vcmpkgs // { fv3 = fv3; }
