#!/bin/bash

# stop on all errors
set -e

# setup run directory
RUNDIR=/rundir
cp /FV3/fv3.exe $RUNDIR/fv3.exe

# setup directory for serialized data
TEST_DATA_DIR=$RUNDIR/test_data
mkdir -p $TEST_DATA_DIR

# setup environment

# run the model (deduce MPI configuration from config file)
cd $RUNDIR
NUM_RANKS=`python ${RUNDIR}/count_ranks.py $RUNDIR/fv3config.yml`
ulimit -s unlimited
echo ">>> Running model, inspect stdout.out / stderr.out in rundir for details"
mpirun -l -np $NUM_RANKS $RUNDIR/fv3.exe 2>$RUNDIR/stderr.out 1>$RUNDIR/stdout.out
echo ">>> Success"

# copy artefacts to test_data directory
cp $RUNDIR/input.nml $TEST_DATA_DIR/
cp $RUNDIR/fortran_sha.txt $TEST_DATA_DIR/
cp $RUNDIR/logfile.*.out $TEST_DATA_DIR/
cp $RUNDIR/std*.out $TEST_DATA_DIR
env > $TEST_DATA_DIR/env.out

# fix permissions (avoids root permission problem outside of container)
# --> use -e USER_ID_HOST=`id -u` -e GROUP_ID_HOST=`id -g` in the docker run command
if [ -n "${USER_ID_HOST}" -a -n "${GROUP_ID_HOST}" ] ; then
    chown -R ${USER_ID_HOST}:${GROUP_ID_HOST} ${RUNDIR}
fi

exit 0
