#!/bin/bash

workdir=/scratch/snx3000/olifu/benchmarking
\rm -rf "${workdir}"
partition="normal --no-wait"

for config in c96 c128 c192 c256 ; do
 for compiler in gnu intel ; do

  exe=/project/s1053/install/fv3gfs-fortran/${compiler}/fv3_64bit.exe
  module_env=/project/s1053/install/fv3gfs-fortran/${compiler}/module.env
  default_args="--timesteps=11 --force --partition=${partition} --executable=${exe} --module_env=${module_env} config/${config}.yml ${workdir}/"
  
  ./run_benchmark.py --hyperthreading --threads_per_rank=4 --nodes_per_tile_side=1 --rank_layout=2 ${default_args}/${config}_${compiler}_yes_4_2x3

 done
done

