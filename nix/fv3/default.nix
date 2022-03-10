{
  stdenv
  , bash
  , call_py_fort
  , fms
  , esmf
  , nceplibs
  , netcdf
  , netcdffortran
  , lapack
  , blas
  , mpich
  , perl
  , gfortran
  , getopt
  , gcovr
} :
let 
  src = builtins.fetchGit {
    url = ../..;
  };
in
stdenv.mkDerivation {
  name = "fv3";
  inherit fms;
  buildInputs = [
      call_py_fort
      call_py_fort.pypkgs.black
      call_py_fort.pypkgs.fv3config
      call_py_fort.pypkgs.numpy
      call_py_fort.pypkgs.pytest
      call_py_fort.pypkgs.pytest-regtest
      call_py_fort.pypkgs.pyyaml
      call_py_fort.pypkgs.xarray
      fms
      esmf
      nceplibs
      netcdf
      netcdffortran
      lapack
      blas
      mpich
      perl
      gfortran
      # needed for gcov utility
      gfortran.cc
      getopt
      gcovr
  ];

  propagatedBuildInputs = [
    mpich
  ];

  srcs = [
    "${src}/FV3"
    "${src}/stochastic_physics"
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
  cp $config conf/configure.fv3
  # ./configure gnu_docker
  cd ..
'';

buildPhase = ''
    make -C FV3 -j 4
'';

installPhase = ''
    PREFIX=$out make -C FV3 install -j 4
'';


  SHELL = "${bash}/bin/bash";
  FMS_DIR="${fms}/include";
  ESMF_DIR="${esmf}";
  LD_LIBRARY_PATH="${esmf}/lib/:${fms}/libFMS/.libs/:$${SERIALBOX_DIR}/lib";
  INCLUDE="-I${fms}/include -I${netcdffortran}/include -I${esmf}/include/";
  NCEPLIBS_DIR="${nceplibs}/lib";
  OMPI_CC="${gfortran.cc}/bin/gcc";
  CALLPYFORT="${call_py_fort}";

  shellHook = ''
    export PYTHONPATH=$(pwd)/tests/emulation:$PYTHONPATH
  '';
}

