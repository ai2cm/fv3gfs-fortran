#!/bin/bash

PREFIX=$1
LDFLAGS=$2


cat << EOF
prefix=${PREFIX}
exec_prefix=\${prefix}
includedir=\${prefix}/include
libdir=\${exec_prefix}/lib

Name: fv3
Description: FV3 and Libraries
Version: 1.0.0
Cflags: -I\${includedir}
Libs: -L\${libdir} ${LDFLAGS} -lfv3cpl -lfv3 -lipd -lfv3core -lfv3coarse_graining -lfv3io -lgfsphys -lstochastic_physics -lFMS -lnetcdff -lnetcdf -llapack -lblas -lesmf 
EOF
