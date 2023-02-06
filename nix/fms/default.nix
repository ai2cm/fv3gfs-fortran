{
  stdenv
  ,bash
  ,rsync
  ,gfortran
  ,mpich
  ,automake
  ,autoconf
  ,m4
  ,libtool
  ,bats
  ,netcdffortran
  ,netcdf
  ,lib
  ,llvmPackages
} :
stdenv.mkDerivation rec {
  pname = "fms";
  version = "0.0.1";

  src = ../../FMS/.;

  # nativeBuildInputs = [ m4 ];
  # buildInputs = [ hdf5 curl mpi ];
  buildInputs = [ bash rsync gfortran mpich automake autoconf m4 libtool bats netcdffortran netcdf 
    (lib.optional stdenv.isDarwin llvmPackages.openmp)
  ];
  inherit netcdffortran;

  configurePhase = ''
    mkdir m4
    autoreconf --install

    export CC=mpicc
    export FC=mpifort
    export LOG_DRIVER_FLAGS="--comments"
    export CPPFLAGS="-Duse_LARGEFILE -DMAXFIELDMETHODS_=500 -DGFS_PHYS"
    export FCFLAGS="-fcray-pointer -Waliasing -ffree-line-length-none -fno-range-check -fdefault-real-8 -fdefault-double-8 -fopenmp -I$netcdffortran/include"

    ./configure --prefix=$out
  '';

}
