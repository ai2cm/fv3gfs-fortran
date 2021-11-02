# Copied from https://nixos.org/nixos/nix-pills/callpackage-design-pattern.html
let
  vcmpkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "vcm-packages";
    url = "git@github.com:VulcanClimateModeling/packages.git";
    ref = "master";
    rev = "f9f66a438833ffc83830fbb0701f4ed656c89bfd";
  });
  # use local version of FMS
  fms = vcmpkgs.callPackage ./nix/fms { };
  fv3 = vcmpkgs.callPackage ./nix/fv3 { fms = fms;};
in vcmpkgs // { fv3 = fv3; }
