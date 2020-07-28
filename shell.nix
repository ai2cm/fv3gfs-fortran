let p=  import ./nix;
in
with import <nixpkgs> {};
mkShell {
  name = "fv3";
  buildInputs = [
      p.fms
      p.esmf
      p.nceplibs
      p.fv3
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
  FMS_DIR="${p.fms}/include";
  ESMF_DIR="${p.esmf}";
  INCLUDE="-I${p.fms}/include -I${netcdffortran}/include -I${p.esmf}/include/";
  NCEPLIBS_DIR="${p.nceplibs}/lib";
  OMPI_CC="${gfortran.cc}/bin/gcc";
}
