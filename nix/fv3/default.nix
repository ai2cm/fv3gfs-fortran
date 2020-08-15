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

srcs = [
  ../../FV3/.
  ../../stochastic_physics/.
];

# need some fancy logic to unpack the two source directories
# https://nixos.org/nixpkgs/manual/#sec-language-texlive-custom-packages
unpackPhase = ''
      runHook preUnpack

      for _src in $srcs; do
        cp -r "$_src" $(stripHash "$_src")
      done

      chmod -R +w .

      runHook postUnpack
'';


config = ./configure.fv3;

configurePhase = ''
    cp $config FV3/conf/configure.fv3

    # need to call this since usr/bin/env isn't 
    # installed in sandboxed build environment
    # there goes 1 hr...
    patchShebangs FV3/mkDepends.pl
'';

buildPhase = ''
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

