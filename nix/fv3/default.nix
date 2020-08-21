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
  , getopt
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
      getopt
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

patchPhase = ''

patchShebangs FV3/configure
  # need to call this since usr/bin/env isn't 
  # installed in sandboxed build environment
  # there goes 1 hr...
  patchShebangs FV3/mkDepends.pl
'';


config = ./configure.fv3;

configurePhase = ''
  cd FV3
  ./configure --prefix="$out"

  cd ..
'';

buildPhase = ''
    make -C FV3 -j 4
'';

installPhase = ''
    make -C FV3 install
'';


  SHELL = "${bash}/bin/bash";
  FMS_DIR="${fms}/include";
  ESMF_DIR="${esmf}";
  LD_LIBRARY_PATH="${esmf}/lib/:${fms}/libFMS/.libs/:$${SERIALBOX_DIR}/lib";
  INCLUDE="-I${fms}/include -I${netcdffortran}/include -I${esmf}/include/";
  NCEPLIBS_DIR="${nceplibs}/lib";
  OMPI_CC="${gfortran.cc}/bin/gcc";
}

