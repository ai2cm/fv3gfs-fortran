# Copied from https://nixos.org/nixos/nix-pills/callpackage-design-pattern.html
let
  overlay = self: super: with nixpkgs; rec {
    # ensure that everything uses mpich for mpi
    fms = self.callPackage ./nix/fms { };
    esmf = self.callPackage ./nix/esmf { };
    nceplibs = self.callPackage ./nix/nceplibs { };
    fv3 = self.callPackage ./nix/fv3 { };
  };

  nixpkgs = import (builtins.fetchTarball {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-as-of-2021-19";
  # this commit has caches on cache.nixos.org, but doesn't update the mpich
  # version from 20.09
  url = "https://github.com/nixos/nixpkgs/archive/7750e6a2c95dd157d4f75a6af00923910870dd5e.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "0mw0w4mk65a6k2v6mdwa5id5rq01sjbx1klcmri9m7i77q7mzd5x";
}) {overlays = [overlay];};
in nixpkgs
