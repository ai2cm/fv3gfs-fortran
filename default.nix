let
  pkgs = import <nixpkgs> {};
  fms=  import ./nix/fms;
  nceplibs=  import ./nix/nceplibs;
  esmf=  import ./nix/esmf;
in
with import <nixpkgs> {}; {
  qpidEnv = stdenv.mkDerivation {
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
        # python
        python37Packages.cython
        python37Packages.setuptools
        python37Packages.numpy
        python37Packages.jinja2
    ];

  src = builtins.fetchGit {
    url = "git@github.com:VulcanClimateModeling/fv3gfs-fortran.git";
  };

  config = ./FV3/conf/configure.fv3;

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
    #LD_LIBRARY_PATH="${esmf}/lib/libO3/Linux.gfortran.64.mpiuni.default/:${fms}/libFMS/.libs/:$${SERIALBOX_DIR}/lib";
    INCLUDE="-I${fms}/include -I${netcdffortran}/include -I${esmf}/mod/modO3/Linux.gfortran.64.mpiuni.default/";
    NCEPLIBS_DIR="${nceplibs}/lib";
    OMPI_CC="${gfortran.cc}/bin/gcc";
};
}

