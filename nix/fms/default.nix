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
{
  stdenv
  ,bash
  ,rsync
  ,gfortran
  ,openmpi
  ,automake
  ,autoconf
  ,m4
  ,libtool
  ,bats
  ,netcdffortran
  ,netcdf
} :
stdenv.mkDerivation rec {
  pname = "fms";
  version = "0.0.0";

  src = builtins.fetchGit{
    url = "git@github.com:VulcanClimateModeling/fv3gfs-fortran.git";
    rev = "c0d11c6a66f76835ae0e9bc791d7cda6d68e504b";
  };


  builder = ./build.sh;

  # nativeBuildInputs = [ m4 ];
  # buildInputs = [ hdf5 curl mpi ];
  buildInputs = [ bash rsync gfortran openmpi automake autoconf m4 libtool bats netcdffortran netcdf 
    (lib.optional stdenv.isDarwin llvmPackages.openmp)
  ];
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
