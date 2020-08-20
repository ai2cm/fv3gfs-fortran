#!/bin/bash

PREFIX=$1
NCEPLIBS=$2


cat << EOF
prefix=${PREFIX}
nceplibs=${NCEPLIBS}
exec_prefix=\${prefix}
includedir=\${prefix}/include
libdir=\${exec_prefix}/lib

Name: fv3
Description: FV3 and Libraries
Version: 1.0.0
Cflags: -I\${includedir}
Libs: -L\${libdir} -lfv3cpl -lfv3 -lipd -lfv3core -lfv3coarse_graining -lfv3io -lgfsphys -lstochastic_physics -L\${nceplibs} -lnemsio_d -lbacio_4 -lsp_v2.0.2_d -lw3emc_d -lw3nco_d
EOF
