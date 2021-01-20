#!/bin/bash

workdir=/scratch/snx3000/olifu/benchmarking
\rm -rf "${workdir}"
#partition="normal --no-wait"
partition="debug"

for config in c96 c128 c192 c256 ; do
for compiler in gnu intel ; do

  exe=/project/s1053/install/fv3gfs-fortran/${compiler}/fv3_64bit.exe
  default_args="--timesteps=11 --force --partition=${partition} --executable=${exe} config/${config}.yml ${workdir}/"
  
  ./run_benchmark.py --no-hyperthreading --threads_per_rank=1 --nodes_per_tile_side=1 --rank_layout=1 ${default_args}/${config}_${compiler}_no_1_1x12
  ./run_benchmark.py --no-hyperthreading --threads_per_rank=1 --nodes_per_tile_side=1 --rank_layout=2 ${default_args}/${config}_${compiler}_no_1_2x6
  ./run_benchmark.py --no-hyperthreading --threads_per_rank=1 --nodes_per_tile_side=1 --rank_layout=3 ${default_args}/${config}_${compiler}_no_1_3x4
  ./run_benchmark.py --no-hyperthreading --threads_per_rank=1 --nodes_per_tile_side=1 --rank_layout=4 ${default_args}/${config}_${compiler}_no_1_4x3
  ./run_benchmark.py --no-hyperthreading --threads_per_rank=1 --nodes_per_tile_side=1 --rank_layout=5 ${default_args}/${config}_${compiler}_no_1_6x2
  ./run_benchmark.py --no-hyperthreading --threads_per_rank=1 --nodes_per_tile_side=1 --rank_layout=6 ${default_args}/${config}_${compiler}_no_1_12x1
  ./run_benchmark.py --hyperthreading    --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=1 ${default_args}/${config}_${compiler}_yes_2_1x12
  ./run_benchmark.py --hyperthreading    --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=2 ${default_args}/${config}_${compiler}_yes_2_2x6
  ./run_benchmark.py --hyperthreading    --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=3 ${default_args}/${config}_${compiler}_yes_2_3x4
  ./run_benchmark.py --hyperthreading    --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=4 ${default_args}/${config}_${compiler}_yes_2_4x3
  ./run_benchmark.py --hyperthreading    --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=5 ${default_args}/${config}_${compiler}_yes_2_6x2
  ./run_benchmark.py --hyperthreading    --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=6 ${default_args}/${config}_${compiler}_yes_2_12x1
  
  ./run_benchmark.py --no-hyperthreading --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=1 ${default_args}/${config}_${compiler}_no_2_1x6
  ./run_benchmark.py --no-hyperthreading --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=2 ${default_args}/${config}_${compiler}_no_2_2x3
  ./run_benchmark.py --no-hyperthreading --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=3 ${default_args}/${config}_${compiler}_no_2_3x2
  ./run_benchmark.py --no-hyperthreading --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=4 ${default_args}/${config}_${compiler}_no_2_6x1
  ./run_benchmark.py --hyperthreading    --threads_per_rank=4 --nodes_per_tile_side=1 --rank_layout=1 ${default_args}/${config}_${compiler}_yes_4_1x6
  ./run_benchmark.py --hyperthreading    --threads_per_rank=4 --nodes_per_tile_side=1 --rank_layout=2 ${default_args}/${config}_${compiler}_yes_4_2x3
  ./run_benchmark.py --hyperthreading    --threads_per_rank=4 --nodes_per_tile_side=1 --rank_layout=3 ${default_args}/${config}_${compiler}_yes_4_3x2
  ./run_benchmark.py --hyperthreading    --threads_per_rank=4 --nodes_per_tile_side=1 --rank_layout=4 ${default_args}/${config}_${compiler}_yes_4_6x1
  
  ./run_benchmark.py --no-hyperthreading --threads_per_rank=4 --nodes_per_tile_side=1 --rank_layout=1 ${default_args}/${config}_${compiler}_no_4_1x3
  ./run_benchmark.py --no-hyperthreading --threads_per_rank=4 --nodes_per_tile_side=1 --rank_layout=2 ${default_args}/${config}_${compiler}_no_4_3x1
  ./run_benchmark.py --hyperthreading    --threads_per_rank=8 --nodes_per_tile_side=1 --rank_layout=1 ${default_args}/${config}_${compiler}_yes_8_1x3
  ./run_benchmark.py --hyperthreading    --threads_per_rank=8 --nodes_per_tile_side=1 --rank_layout=2 ${default_args}/${config}_${compiler}_yes_8_3x1

  exit 0

done
done

