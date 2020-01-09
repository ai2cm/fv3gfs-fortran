#!/bin/bash

set +e
\rm fv3*.exe
set -e

# set exe
if [ -z "${EXE_PATH}" ]; then
    echo "EXE_PATH is not set"
    exit 1
else
    echo "EXE_PATH=${EXE_PATH}"
fi

# set target
if [ "${TARGET}" == "GPU" ]; then
    target=gpu
else
    target=cpu
fi
echo "target=${TARGET}"

# set real type
if [ "${REAL_TYPE}" == "single" ]; then
    real_type=sp
else
    real_type=dp
fi
echo "real_type=${real_type}"

# check whether to run only a few tests
if [ ! -z "${ONLY}" ]; then
    only="--only=${ONLY}"
    echo "only=${ONLY}"
fi

# check number of steps to run
if [ ! -z "${STEPS}" ]; then
    steps="--steps=${STEPS}"
    echo "steps=${STEPS}"
fi

if [ ! -z "${TUNE}" ]; then
    tune="--tune-thresholds"
    echo "tuning tresholds"
fi

# check exe
echo "ldd ${EXE_PATH}"
ldd "${EXE_PATH}"

# copy exe
echo "copying ${EXE_PATH} to fv3.exe"
cp "${EXE_PATH}" fv3.exe

# testsuite location and configuration
testsuite="./src/testsuite.py"
config="--config-file=config_fv3.yaml"

# remove old output
\rm -f testsuite.out

# ready for liftoff!
echo "running ${testsuite} with configuration ${config}"

