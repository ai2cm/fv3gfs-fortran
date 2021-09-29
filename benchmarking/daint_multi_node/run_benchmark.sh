#!/bin/bash

workdir=/scratch/snx3000/olifu/benchmarking
#\rm -rf "${workdir}"
partition="normal"

for nodes_per_tile_side in 1 2 4 6 8 10 12 14 16 18 20 ; do
  for config in c128 c192 c256 c384 ; do
    for compiler in intel ; do
   
      srcdir=/project/s1053/install/fv3gfs-fortran/${compiler}
      exe=${srcdir}/fv3_64bit.exe
      module_env=${srcdir}/module.env
      git_env=${srcdir}/git.env
      args="--hyperthreading --threads_per_rank=4 --nodes_per_tile_side=${nodes_per_tile_side} --rank_layout=-1 --timesteps=11 --force --partition=${partition} --no-wait --executable=${exe} --module_env=${module_env} config/${config}.yml"
      rundir=${workdir}/${config}_${compiler}_${nodes_per_tile_side}_yes_4
      if [ ! -d "${rundir}" ] ; then
        ./run_benchmark.py ${args} ${rundir}
        cp ${git_env} ${rundir}
      fi

    done
  done
done

