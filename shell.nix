let
  pkgs = import <nixpkgs> {};
  fms=  import ./nix/fms;
  nceplibs=  import ./nix/nceplibs;
  esmf=  import ./nix/esmf;
in
with import <nixpkgs> {}; {
  qpidEnv = stdenv.mkDerivation {
    name = "my-gcc8-environment";
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
        gfortran.cc
        # python
        python37Packages.cython
        python37Packages.setuptools
        python37Packages.numpy
        python37Packages.jinja2
    ];

  src = builtins.fetchGit {
    url = "git@github.com:VulcanClimateModeling/fv3gfs-fortran.git";
    rev = "c0d11c6a66f76835ae0e9bc791d7cda6d68e504b";
  };

  config = ./FV3/conf/configure.fv3;

  buildPhase = ''
      ls
      ls FV3
      cp $config FV3/conf/configure.fv3
      make -C FV3 libs_no_dycore
      make -C FV3/atmos_cubed_sphere
      make -C FV3
  '';


    FMS_DIR="${fms}/include";
    ESMF_DIR="${esmf}";
    #LD_LIBRARY_PATH="${esmf}/lib/libO3/Linux.gfortran.64.mpiuni.default/:${fms}/libFMS/.libs/:$${SERIALBOX_DIR}/lib";
    INCLUDE="-I${fms}/include -I${netcdffortran}/include -I${esmf}/mod/modO3/Linux.gfortran.64.mpiuni.default/";
    NCEPLIBS_DIR="${nceplibs}/lib";
    OMPI_CC="${gfortran.cc}/bin/gcc";
};
}

