#!/bin/bash

PREFIX=$1
LDFLAGS=$2
CFLAGS=$3


echo $CFLAGS

cat << EOF
prefix=${PREFIX}
exec_prefix=\${prefix}
includedir=\${prefix}/include
libdir=\${exec_prefix}/lib

Name: fv3
Description: FV3 and Libraries
Version: 1.0.0
Cflags: ${CFLAGS} -I${PREFIX}/include
Libs: -L\${libdir} ${LDFLAGS}
EOF
# librt is needed for ESMF
