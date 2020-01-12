#!/bin/bash

HEAD=$1
RUNDIR=$HEAD/rundir
ulimit -s unlimited
cp $HEAD/fv3.exe $RUNDIR/fv3.exe
cd $RUNDIR
mpirun -np 6 --allow-run-as-root --mca btl_vader_single_copy_mechanism none --oversubscribe $RUNDIR/fv3.exe
