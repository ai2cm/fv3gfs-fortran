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
    libopenmpi3 \
    bats

# download and install NCEP libraries
RUN git config --global http.sslverify false && \
    git clone https://github.com/NCAR/NCEPlibs.git && \
    mkdir /opt/NCEPlibs && \
    cd NCEPlibs && \
    git checkout 3da51e139d5cd731c9fc27f39d88cb4e1328212b && \
    echo "y" | ./make_ncep_libs.sh -s linux -c gnu -d /opt/NCEPlibs -o 1


## Build FMS
##---------------------------------------------------------------------------------
FROM fv3gfs-environment AS fv3gfs-fms

ENV CC=mpicc
ENV FC=mpifort
ENV LDFLAGS="-L/usr/lib"
ENV LOG_DRIVER_FLAGS="--comments"
ENV CPPFLAGS="-I/usr/include -Duse_LARGEFILE -DMAXFIELDMETHODS_=500"
ENV FCFLAGS="-fcray-pointer -Waliasing -ffree-line-length-none -fno-range-check -fdefault-real-8 -fdefault-double-8 -fopenmp"

COPY FMS /FMS
RUN cd /FMS && autoreconf --install && ./configure 
RUN cd /FMS && make -j8
RUN mv /FMS/*/*.mod /FMS/*/*.o /FMS/*/*.h FMS/

## Build ESMF
##---------------------------------------------------------------------------------
FROM fv3gfs-environment AS fv3gfs-esmf

ENV ESMF_DIR=/esmf
ENV ESMF_INSTALL_PREFIX=/usr/local/esmf
ENV ESMF_NETCDF_INCLUDE=/usr/include
ENV ESMF_NETCDF_LIBS="-lnetcdf -lnetcdff"
ENV ESMF_BOPT=O3

RUN git clone -b ESMF_8_0_0 --depth 1 https://git.code.sf.net/p/esmf/esmf $ESMF_DIR
RUN cd $ESMF_DIR && make lib -j8 && make install && make installcheck

## Build FV3 executable in its own image
##---------------------------------------------------------------------------------
FROM fv3gfs-environment AS fv3gfs-build

ENV FMS_DIR=/FMS
ENV ESMF_DIR=/usr/local/esmf
ENV ESMF_INC="-I/usr/local/esmf/include -I${ESMF_DIR}/mod/modO3/Linux.gfortran.64.mpiuni.default/"
ENV LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${ESMF_DIR}/lib/libO3/Linux.gfortran.64.mpiuni.default/:${FMS_DIR}/libFMS/.libs/

COPY --from=fv3gfs-fms /FMS $FMS_DIR
COPY --from=fv3gfs-esmf /usr/local/esmf ${ESMF_DIR}

COPY stochastic_physics /stochastic_physics
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

ARG compile_option
ARG configure_file=configure.fv3.gnu_docker

# copy appropriate configuration file to configure.fv3
RUN cp /FV3/conf/$configure_file \
        /FV3/conf/configure.fv3 && \
    if [ ! -z $compile_option ]; then sed -i "33i $compile_option" \
        /FV3/conf/configure.fv3; fi

RUN cd /FV3 && make clean_no_dycore && make libs_no_dycore -j8

COPY FV3/atmos_cubed_sphere /FV3/atmos_cubed_sphere

RUN cd /FV3/atmos_cubed_sphere && make clean && cd /FV3 && make -j8


## Define final "user" image for running tests
##---------------------------------------------------------------------------------
FROM fv3gfs-build AS fv3gfs-compiled

RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip

# enable python/pip (as opposed to python3/pip3)
RUN ln -s /bin/python3 /bin/python && \
    ln -s /bin/pip3 /bin/pip && \
    pip install --no-cache-dir pyyaml

COPY FV3/testsuite /FV3/testsuite

# run model
CMD ["bash", "/FV3/rundir/submit_job.sh"]
