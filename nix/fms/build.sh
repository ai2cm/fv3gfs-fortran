source $stdenv/setup

pwd

export CC=mpicc
export FC=mpifort
#export LDFLAGS="-L$netcdf/lib"
export LOG_DRIVER_FLAGS="--comments"
export CPPFLAGS="-Duse_LARGEFILE -DMAXFIELDMETHODS_=500 -DGFS_PHYS"
export FCFLAGS="-fcray-pointer -Waliasing -ffree-line-length-none -fno-range-check -fdefault-real-8 -fdefault-double-8 -fopenmp -I$netcdffortran/include"

cp -r $src/FMS src
chmod -R +w src
cd src

mkdir m4

autoreconf --install
./configure --prefix=$out
make -j8
make install
#mv /FMS/*/*.mod /FMS/*/*.o /FMS/*/*.h /FMS/

