with import <nixpkgs> {};
# { stdenv
# , netcdffortran
# , perl
# , gfortran
# , lapack
# , blas
# , openmpi
# , fetchgit
# }:
# 
stdenv.mkDerivation rec {
  pname = "esmf";
  version = "0.0.0";

  src = builtins.fetchGit {
    url = "https://git.code.sf.net/p/esmf/esmf";
    rev = "f5d862d2ec066e76647f53c239b8c58c7af28e45";
  };


  builder = ./build.sh;

  # nativeBuildInputs = [ m4 ];
  # buildInputs = [ hdf5 curl mpi ];
  buildInputs = [ netcdffortran gfortran openmpi gfortran.cc ];
  inherit netcdffortran;



  meta = {
      description = "Libraries for the Unidata network Common Data Format";
      platforms = stdenv.lib.platforms.unix;
      homepage = "https://www.unidata.ucar.edu/software/netcdf/";
      license = {
        url = "https://www.unidata.ucar.edu/software/netcdf/docs/copyright.html";
      };
  };
}
