#!/bin/bash

set -e

RUNDIR=/rundir
TEST_DATA_DIR=$RUNDIR/test_data
NUM_RANKS=`python ${RUNDIR}/count_ranks.py $RUNDIR/fv3config.yml`

cp /FV3/fv3.exe $RUNDIR/fv3.exe
mkdir -p $TEST_DATA_DIR
cp $RUNDIR/input.nml $TEST_DATA_DIR/
cp $RUNDIR/fortran_sha.txt $TEST_DATA_DIR/

cd $RUNDIR

ulimit -s unlimited
mpirun -np $NUM_RANKS $RUNDIR/fv3.exe

exit 0
