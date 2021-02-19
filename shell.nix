with import <nixpkgs> { };
let
  p = import ./nix;
  fv3 = with python3Packages; callPackage ./python-packages.nix { };
  python = python3.override {
    packageOverrides = fv3;
  };
  py = python.withPackages (ps: [ ps.pytest ps.pyyaml ps.fv3config ]);
in mkShell {
  name = "fv3";
  buildInputs = [
    p.fms
    p.esmf
    p.nceplibs
    p.fv3
    py
    netcdf
    netcdffortran
    openssh # hidden dependency of openmpi
    lapack
    blas
    openmpi
    perl
    gfortran
    vim
  ];

  SHELL = "${bash}/bin/bash";
  FMS_DIR = "${p.fms}/include";
  ESMF_DIR = "${p.esmf}";
  INCLUDE =
    "-I${p.fms}/include -I${netcdffortran}/include -I${p.esmf}/include/";
  NCEPLIBS_DIR = "${p.nceplibs}/lib";
  OMPI_CC = "${gfortran.cc}/bin/gcc";
}
