#!/bin/bash

N_PROCESSES=$1
RUNDIR=/rundir

ulimit -s unlimited
cp /FV3/fv3.exe $RUNDIR/fv3.exe
cd $RUNDIR
mpirun -np $N_PROCESSES $RUNDIR/fv3.exe
