#!/bin/bash

if [ "$#" -lt 1 ]; then
    echo "ERROR: You must supply a docker tag (default/32bit)."
    exit 1
fi

export COMPILED_TAG_NAME=$1
export COMPILED_IMAGE=us.gcr.io/vcm-ml/fv3gfs-compiled-$COMPILED_TAG_NAME

[[ -d inputdata ]] || ./download_inputdata.sh
# bash tests/test_output_netcdfs_identical.sh &&
# bash tests/test_regression_fails_for_900s_timestep.sh
TARGET=cpu REAL_TYPE=double COMPILED_IMAGE=$COMPILED_IMAGE bash tests/testsuite.sh
