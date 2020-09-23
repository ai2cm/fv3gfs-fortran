ARG serialize=false
ARG FMS_IMAGE=fv3gfs-fms
ARG ESMF_IMAGE=fv3gfs-esmf
ARG SERIALBOX_IMAGE=fv3gfs-environment-serialbox

FROM ubuntu:19.10 AS fv3gfs-environment

RUN apt-get update && apt-get install -y \
    curl \
    wget \
    gcc \
    git \
    libblas-dev \
    liblapack-dev \
    libnetcdf-dev \
    libnetcdff-dev \
    perl \
    make \
    rsync \
    libffi-dev \
    openssl \
    bats

RUN wget -q http://www.mpich.org/static/downloads/3.1.4/mpich-3.1.4.tar.gz && \
    tar xzf mpich-3.1.4.tar.gz && \
    cd mpich-3.1.4 && \
    ./configure --enable-fortran --enable-cxx --prefix=/usr --enable-fast=all,O3 && \
    make -j24 && make install && ldconfig && \
    cd .. && rm -rf mpich-3.1.4*

# download and install NCEP libraries
RUN git config --global http.sslverify false && \
    git clone https://github.com/NCAR/NCEPlibs.git && \
    mkdir /opt/NCEPlibs && \
    cd NCEPlibs && \
    git checkout 3da51e139d5cd731c9fc27f39d88cb4e1328212b && \
    echo "y" | ./make_ncep_libs.sh -s linux -c gnu -d /opt/NCEPlibs -o 1

##
## Setup environment for Serialbox
##---------------------------------------------------------------------------------
FROM fv3gfs-environment as fv3gfs-environment-serialbox
# set TZ
ENV DEBIAN_FRONTEND=noninteractive TZ=US/Pacific
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# install dependencies for serialbox
RUN apt-get update && apt-get install -y \
    cmake \
    libssl-dev \
    clang \
    clang-format \
    clang-tidy \
    python3-numpy \
    python3-nose \
    python3-sphinx

# Install headers from the Boost library
RUN wget -q https://dl.bintray.com/boostorg/release/1.74.0/source/boost_1_74_0.tar.gz && \
    tar xzf boost_1_74_0.tar.gz && \
    cd boost_174_0 && \
    cp -r boost /usr/include/ && cd /    

# install serialbox2 from source
COPY serialbox2 serialbox2
RUN cd serialbox2 && \
    mkdir build && \
    cd build && \
    cmake -DSERIALBOX_USE_NETCDF=ON -DSERIALBOX_ENABLE_FORTRAN=ON \
      -DSERIALBOX_TESTING=ON  ../ && \
    make -j16 && \
    make test && \
    make install

## Build FMS
##---------------------------------------------------------------------------------
FROM fv3gfs-environment AS fv3gfs-fms

ENV CC=/usr/bin/mpicc \
    FC=/usr/bin/mpif90 \
    LDFLAGS="-L/usr/lib" \
    LOG_DRIVER_FLAGS="--comments" \
    CPPFLAGS="-I/usr/include -Duse_LARGEFILE -DMAXFIELDMETHODS_=500 -DGFS_PHYS" \
    FCFLAGS="-fcray-pointer -Waliasing -ffree-line-length-none -fno-range-check -fdefault-real-8 -fdefault-double-8 -fopenmp"

COPY FMS /FMS
RUN cd /FMS && autoreconf --install && ./configure && \
    cd /FMS && make -j8 && \
    mv /FMS/*/*.mod /FMS/*/*.o /FMS/*/*.h /FMS/

## Build ESMF
##---------------------------------------------------------------------------------
FROM fv3gfs-environment AS fv3gfs-esmf

ENV ESMF_DIR=/esmf \
    ESMF_INSTALL_PREFIX=/usr/local/esmf \
    ESMF_INSTALL_MODDIR=/usr/local/esmf/include \
    ESMF_INSTALL_HEADERDIR=/usr/local/esmf/include \
    ESMF_INSTALL_LIBDIR=/usr/local/esmf/lib \
    ESMF_INSTALL_BINDIR=/usr/local/esmf/bin \
    ESMF_NETCDF_INCLUDE=/usr/include \
    ESMF_NETCDF_LIBS="-lnetcdf -lnetcdff" \
    ESMF_BOPT=O3

RUN git clone -b ESMF_8_0_0 --depth 1 https://git.code.sf.net/p/esmf/esmf $ESMF_DIR && \
    cd $ESMF_DIR && \
    make lib -j24 && \
    make install && \
    make installcheck

## Copy FV3GFS sources for non-serialize image
##---------------------------------------------------------------------------------
FROM ubuntu:19.10 AS fv3gfs-src-serialize-false

ARG compile_option
ARG configure_file=configure.fv3.gnu_docker

COPY stochastic_physics /stochastic_physics
COPY FV3/coarse_graining /FV3/coarse_graining
COPY FV3/conf /FV3/conf
COPY FV3/ccpp /FV3/ccpp
COPY FV3/cpl /FV3/cpl
COPY FV3/gfsphysics /FV3/gfsphysics
COPY FV3/io /FV3/io
COPY FV3/ipd /FV3/ipd
COPY FV3/stochastic_physics /FV3/stochastic_physics
COPY FV3/makefile FV3/mkDepends.pl FV3/atmos_model.F90 FV3/LICENSE.md \
    FV3/coupler_main.F90 FV3/fv3_cap.F90 FV3/module_fcst_grid_comp.F90 \
    FV3/module_fv3_config.F90 FV3/time_utils.F90 /FV3/

# copy appropriate configuration file to configure.fv3
RUN cp /FV3/conf/$configure_file \
        /FV3/conf/configure.fv3 && \
    if [ ! -z $compile_option ]; then sed -i "33i $compile_option" \
        /FV3/conf/configure.fv3; fi

COPY FV3/atmos_cubed_sphere /FV3/atmos_cubed_sphere

## Convert FV3GFS sources for serialize image
##---------------------------------------------------------------------------------
FROM $SERIALBOX_IMAGE AS fv3gfs-src-serialize-true

ARG compile_option
ARG configure_file=configure.fv3.gnu_docker_serialize

ENV SERIALBOX_OUTDIR=/FV3
ENV FV3_BASE=/FV3/original

COPY --from=fv3gfs-src-serialize-false /FV3 $FV3_BASE
COPY --from=fv3gfs-src-serialize-false /stochastic_physics $(FV3_BASE)/../stochastic_physics 

# copy appropriate configuration file to configure.fv3
RUN cp $FV3_BASE/conf/$configure_file \
        $FV3_BASE/conf/configure.fv3 && \
    if [ ! -z $compile_option ]; then sed -i "33i $compile_option" \
        $FV3_BASE/conf/configure.fv3; fi && \
    cd $FV3_BASE && \
    make serialize_preprocess

FROM fv3gfs-src-serialize-$serialize AS fv3gfs-sources

FROM $FMS_IMAGE AS fms_image
FROM $ESMF_IMAGE AS esmf_image
FROM $SERIALBOX_IMAGE AS serialbox_image

## Build FV3 executable in its own image
##---------------------------------------------------------------------------------
FROM fv3gfs-environment AS fv3gfs-build

ENV SERIALBOX_DIR=/serialbox2 \
    SERIALBOX_OUTDIR=/FV3 \
    FMS_DIR=/FMS \
    ESMF_DIR=/usr/local/esmf

ENV ESMF_INC="-I/usr/local/esmf/include" \
    LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${ESMF_DIR}/lib:${FMS_DIR}/libFMS/.libs/:${SERIALBOX_DIR}/lib \
    PPSER_PY=$(SERIALBOX_DIR)/python/pp_ser/pp_ser.py

COPY --from=serialbox_image /serialbox2/install $SERIALBOX_DIR
COPY --from=fms_image /FMS $FMS_DIR
COPY --from=esmf_image /usr/local/esmf $ESMF_DIR

COPY --from=fv3gfs-sources /FV3 /FV3
COPY --from=fv3gfs-sources /stochastic_physics /stochastic_physics

RUN cd /FV3 && make clean_no_dycore && make libs_no_dycore -j16

COPY --from=fv3gfs-sources FV3/atmos_cubed_sphere /FV3/atmos_cubed_sphere

RUN cd /FV3/atmos_cubed_sphere && make clean && cd /FV3 && make -j16

## Define final "user" image for running tests
##---------------------------------------------------------------------------------
FROM fv3gfs-build AS fv3gfs-compiled

RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip && \
    ln -s /bin/python3 /bin/python && \
    ln -s /bin/pip3 /bin/pip && \
    pip install --no-cache-dir pyyaml

COPY FV3/testsuite /FV3/testsuite

# run model
CMD ["bash", "/rundir/submit_job.sh"]
