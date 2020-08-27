#!/bin/bash

RUNDIR=/rundir
TEST_DATA_DIR=$RUNDIR/test_data
if [ -z $NUM_RANKS ]; then
    NUM_RANKS=6
fi
ulimit -s unlimited
cp /FV3/fv3.exe $RUNDIR/fv3.exe
mkdir -p $TEST_DATA_DIR
cp $RUNDIR/input.nml $TEST_DATA_DIR/
cp $RUNDIR/fortran_sha.txt $TEST_DATA_DIR/
cd $RUNDIR
mpirun -np $NUM_RANKS --allow-run-as-root --mca btl_vader_single_copy_mechanism none --oversubscribe $RUNDIR/fv3.exe
