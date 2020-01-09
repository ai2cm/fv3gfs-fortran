#!/bin/bash

# be loud and exit upon error
set -e
set -x

# setup some defaults and check sanity of environment
if [ -z "${COMPILED_IMAGE}" ]; then
  COMPILED_IMAGE="fv3gfs-compiled-default"
fi
if [ -z "${EXE_PATH}" ]; then
  EXE_PATH="../../fv3.exe"
fi
if [ -z "${TARGET}" ]; then
  TARGET=cpu
fi
if [ -z "${REAL_TYPE}" ]; then
  REAL_TYPE=double
fi
if [ ! -z "${ONLY}" ]; then
  only="--env ONLY=${ONLY}"
fi
if [ ! -z "${STEPS}" ]; then
  steps="--env STEPS=${STEPS}"
fi

# run the testsuite inside the docker container
docker run \
    --rm \
    --env TARGET="${TARGET}" \
    --env EXE_PATH=${EXE_PATH} \
    --env REAL_TYPE="${REAL_TYPE}" \
    ${only} ${steps} \
    -it $COMPILED_IMAGE \
    /bin/bash -c 'cd /FV3/testsuite; ./data/get_data.sh; ./submit.docker.sh'

