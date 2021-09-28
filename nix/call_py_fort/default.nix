{ fetchFromGitHub, lib, stdenv, gfortran, cmake, llvmPackages, python3Packages }:
stdenv.mkDerivation {
  name = "call_py_fort";

  src = fetchFromGitHub {
    owner = "nbren12";
    repo = "call_py_fort";
    rev = "f1d85c5e15756af583f30748b8a5bb1ff173a8e5";
    sha256 = "sha256-mWyO6GC3iiVYoC8fksnDJsEtwTLawGqtQ/v8Ar6cV8I";
  };

  nativeBuildInputs = lib.optional stdenv.isDarwin llvmPackages.openmp;
  buildInputs = [ gfortran gfortran.cc.lib cmake gfortran.cc ];
  propagatedBuildInputs = [ python3Packages.cffi python3Packages.numpy ];
  doCheck = false;
  
  passthru = {
    pythonPackages = python3Packages;
  };

  preCheck = ''
    export PYTHONPATH=$(pwd)/../test:$PYTHONPATH
    # for mac
    export DYLD_LIBRARY_PATH=$(pwd)/src
    # for linux
    export LD_LIBRARY_PATH=$(pwd)/src
  '';
  shellHook = ''
    export PYTHONPATH=$(pwd)/test:$PYTHONPATH
    # for mac
    export DYLD_LIBRARY_PATH=$(pwd)/src
    # for linux
    export LD_LIBRARY_PATH=$(pwd)/src
  '';
}
