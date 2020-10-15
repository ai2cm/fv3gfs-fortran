#!/bin/bash

RUNDIR=/rundir

ulimit -s unlimited
cp /FV3/fv3.exe $RUNDIR/fv3.exe
cd $RUNDIR
mpirun -np 6 $RUNDIR/fv3.exe
