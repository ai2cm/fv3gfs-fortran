#!/bin/bash

# setup
RUN_DIR_HOST=`pwd`/rundir
RUN_DIR_CONTAINER=/rundir
DATA_DIR_HOST=`pwd`/data
DATA_DIR_CONTAINER=/data
USER_ID_HOST=`id -u`
GROUP_ID_HOST=`id -g`
COMPILED_IMAGE=us.gcr.io/vcm-ml/fv3gfs-compiled:physics-serialize

# prepare run directory
cd ${RUN_DIR_HOST}
./clean.sh
cd -

# prepare data directory
mkdir -p ${DATA_DIR_HOST}

# run model and serialize data
docker run -e USER_ID_HOST=${USER_ID_HOST} -e GROUP_ID_HOST=${GROUP_ID_HOST} \
        -e SER_ENV=${SER_ENV} \
        --network host --rm -v ${RUN_DIR_HOST}:${RUN_DIR_CONTAINER} \
        -v ${DATA_DIR_HOST}:${DATA_DIR_CONTAINER} \
        --mount type=tmpfs,destination=${RUN_DIR_CONTAINER}/test_data \
        --shm-size=512m \
        ${COMPILED_IMAGE} /rundir/submit_job.sh

