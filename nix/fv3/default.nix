{
  stdenv
  , bash
  , fms
  , esmf
  , nceplibs
  , netcdf
  , netcdffortran
  , lapack
  , blas
  , openmpi
  , perl
  , gfortran
} :
stdenv.mkDerivation {
  name = "fv3";
  buildInputs = [
      fms
      esmf
      nceplibs
      netcdf
      netcdffortran
      lapack
      blas
      openmpi
      perl
      gfortran
  ];

src = builtins.fetchGit {
  url = "git@github.com:VulcanClimateModeling/fv3gfs-fortran.git";
};

config = ./configure.fv3;

buildPhase = ''
    
    cp $config FV3/conf/configure.fv3

    # need to call this since usr/bin/env isn't 
    # installed in sandboxed build environment
    # there goes 1 hr...
    patchShebangs FV3/mkDepends.pl

    make -C FV3 -j 4
'';

installPhase = ''
  mkdir -p $out/bin $out/lib 
  cp FV3/*.exe $out/bin
  cp FV3/*.o $out/lib
'';


  SHELL = "${bash}/bin/bash";
  FMS_DIR="${fms}/include";
  ESMF_DIR="${esmf}";
  LD_LIBRARY_PATH="${esmf}/lib/:${fms}/libFMS/.libs/:$${SERIALBOX_DIR}/lib";
  INCLUDE="-I${fms}/include -I${netcdffortran}/include -I${esmf}/include/";
  NCEPLIBS_DIR="${nceplibs}/lib";
  OMPI_CC="${gfortran.cc}/bin/gcc";
}

