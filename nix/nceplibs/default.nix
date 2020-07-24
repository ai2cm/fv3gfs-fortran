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
stdenvNoCC.mkDerivation rec {
  pname = "nceplibs";
  version = "0.0.0";

  src = fetchgit {
    url = "https://github.com/NCAR/NCEPlibs.git";
    rev = "3da51e139d5cd731c9fc27f39d88cb4e1328212b";
    sha256 = "03yjjz34452gbq292x24lvgxj4ybasm8z2aqlps0l6hg04blkhji";
    fetchSubmodules = false;
    leaveDotGit = true;
  };


  builder = ./build.sh;

  # nativeBuildInputs = [ m4 ];
  # buildInputs = [ hdf5 curl mpi ];
  buildInputs = [ bash rsync gfortran openmpi coreutils ];
  FFLAGS="-I${gfortran.libc}/include";
  arch = if system == "x86_64-darwin" then "macosx" else "linux";



  meta = {
      description = "Libraries for the Unidata network Common Data Format";
      platforms = stdenv.lib.platforms.unix;
      homepage = "https://www.unidata.ucar.edu/software/netcdf/";
      license = {
        url = "https://www.unidata.ucar.edu/software/netcdf/docs/copyright.html";
      };
  };
}
