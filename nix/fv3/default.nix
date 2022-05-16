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
  fetchPypi = call_py_fort.pypkgs.fetchPypi;
  pace-util = with call_py_fort.pypkgs ; buildPythonPackage rec {
    pname = "pace-util";
    version = "0.7.0";
    src = fetchPypi {
      inherit pname version;
      sha256 = "sha256-GBfdbryL0ylSDFefoJCobpFFJc1tfAaQ1gjeK0+BOvg=";
    };
    propagatedBuildInputs = [
      zarr
      xarray
      cftime
      numpy
      fsspec
      typing-extensions
    ];
    # doesn't find pytest, not sure why, disabling tests for now.
    doCheck = false;
  };
  fortls = with call_py_fort.pypkgs ; buildPythonApplication rec {
    pname = "fortls";
    version = "2.5.0";
    src = fetchPypi {
      inherit pname version;
      sha256 = "sha256-LLal4WwEFfvfhscJf0IHMCQ7l0LRiSGnxua5lsi9NtM=";
    };
    propagatedBuildInputs = [
      packaging
    ];
    # doesn't find pytest, not sure why, disabling tests for now.
    doCheck = false;
  };
  fv3config = let version ="0.9.0";
  in
  call_py_fort.pypkgs.fv3config.overridePythonAttrs (attrs :{
    version = version;
    src = fetchPypi {
      version = version;
      pname = attrs.pname;
      sha256 = "sha256-iqJdIXQChmiM3hDVcJpV8gc+SoAOSaPGJ6OWuSdzQ0Y=";
    };
  });
in
stdenv.mkDerivation {
  name = "fv3";
  inherit fms;
  buildInputs = [
      call_py_fort
      call_py_fort.pypkgs.black
      call_py_fort.pypkgs.numpy
      call_py_fort.pypkgs.pytest
      call_py_fort.pypkgs.pytest-regtest
      call_py_fort.pypkgs.pyyaml
      call_py_fort.pypkgs.xarray
      call_py_fort.pypkgs.cython
      call_py_fort.pypkgs.mpi4py
      call_py_fort.pypkgs.wheel
      # dev tooling
      call_py_fort.pypkgs.pre-commit
      # read the docs
      call_py_fort.pypkgs.sphinx
      call_py_fort.pypkgs.sphinx_rtd_theme
      # fof
      fv3config
      pace-util
      fortls
      # avoids linking agains static libgfortran
      gfortran.cc.lib
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


configurePhase = ''
  cd FV3
  ./configure nix

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
    export PYTHONPATH=$(pwd)/tests/emulation:$(pwd)/FV3/wrapper:$PYTHONPATH
    # path to fv3.exe
    export PATH=$(pwd)/FV3:$PATH
  '';
}

