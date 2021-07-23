# Copied from https://nixos.org/nixos/nix-pills/callpackage-design-pattern.html
let
  vcmpkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "vcm-packages";
    url = "git@github.com:VulcanClimateModeling/packages.git";
    ref = "master";
    rev = "72783667a7273a40080790330a1c736ae5ac02a5";
  });
  # use local version of FMS
  fms = vcmpkgs.callPackage ./nix/fms { };
  fv3 = vcmpkgs.callPackage ./nix/fv3 { fms = fms;};
in vcmpkgs // { fv3 = fv3; }
