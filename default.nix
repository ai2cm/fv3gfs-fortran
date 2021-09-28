# Copied from https://nixos.org/nixos/nix-pills/callpackage-design-pattern.html
let
  overlay = self: super:
    with nixpkgs; rec {
      # ensure that everything uses mpich for mpi
      fms = self.callPackage ./nix/fms { };
      esmf = self.callPackage ./nix/esmf { };
      nceplibs = self.callPackage ./nix/nceplibs { };
      fv3 = self.callPackage ./nix/fv3 { };
      call_py_fort = self.callPackage ./nix/call_py_fort { };
    };

  nixpkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "release-21.05-pre";
    url =
      "https://github.com/nixos/nixpkgs/archive/377de4787e4a2407ba74f592a5dbfbe2dec99486.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "sha256:12r9ldlwb6z0dlqh2d57hjslkh3gmx6101ab7m7lribwb6midbds";
  }) { overlays = [ overlay ]; };
in nixpkgs
