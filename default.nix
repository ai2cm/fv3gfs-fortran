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
  name = "release-20.09";
  # Commit hash for nixos-unstable as of 2018-09-12
  url = "https://github.com/nixos/nixpkgs/archive/20.09.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
}) {overlays = [overlay];};
in nixpkgs