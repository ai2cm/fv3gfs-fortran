#!/bin/bash

set -e
set -x

modelType=$COMPILED_TAG_NAME
COMPILED_IMAGE=us.gcr.io/vcm-ml/fv3gfs-compiled:$COMPILED_TAG_NAME
originalCheckSum=$(pwd)/tests/original_checksum_default.txt
experiment=different_dt

# needed for makefile commands
export EXPERIMENT=$experiment

rundir_host=$PWD/experiments/$experiment/rundir
rundir_container=/FV3/rundir
fixdatadir_host=$PWD/inputdata/fv3gfs-data-docker/fix.v201702
fixdatadir_container=/inputdata/fix.v201702

bash create_case.sh -f $experiment
# # copy different namelist in
cp -f tests/input_different_dt.nml $rundir_host/input.nml
docker run \
    --rm \
    -v $rundir_host:$rundir_container \
    -v $fixdatadir_host:$fixdatadir_container \
    -it $COMPILED_IMAGE

echo "Checking md5sum. This should differ"
(
    cd experiments/$experiment/rundir
    md5sum -c $originalCheckSum
) || exit 0
# This will exit 0 (i.e. OK) if the md5sum fails the checks
