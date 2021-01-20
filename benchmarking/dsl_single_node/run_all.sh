#!/bin/bash

default_args="--timesteps=11 --force --partition=debug --executable=/users/olifu/vulcan/fv3gfs-fortran/FV3/fv3_64bit.exe config/c96.yml /scratch/snx3000/olifu/benchmarking/"

./run_benchmark.py --no-hyperthreading --threads_per_rank=1 --nodes_per_tile_side=1 --rank_layout=1 ${default_args}/gnu_no_1_01x12
./run_benchmark.py --no-hyperthreading --threads_per_rank=1 --nodes_per_tile_side=1 --rank_layout=2 ${default_args}/gnu_no_1_02x06
./run_benchmark.py --no-hyperthreading --threads_per_rank=1 --nodes_per_tile_side=1 --rank_layout=3 ${default_args}/gnu_no_1_03x04
./run_benchmark.py --no-hyperthreading --threads_per_rank=1 --nodes_per_tile_side=1 --rank_layout=4 ${default_args}/gnu_no_1_04x03
./run_benchmark.py --no-hyperthreading --threads_per_rank=1 --nodes_per_tile_side=1 --rank_layout=5 ${default_args}/gnu_no_1_06x02
./run_benchmark.py --no-hyperthreading --threads_per_rank=1 --nodes_per_tile_side=1 --rank_layout=6 ${default_args}/gnu_no_1_12x01
./run_benchmark.py --hyperthreading    --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=1 ${default_args}/gnu_yes_2_01x12
./run_benchmark.py --hyperthreading    --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=2 ${default_args}/gnu_yes_2_02x06
./run_benchmark.py --hyperthreading    --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=3 ${default_args}/gnu_yes_2_03x04
./run_benchmark.py --hyperthreading    --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=4 ${default_args}/gnu_yes_2_04x03
./run_benchmark.py --hyperthreading    --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=5 ${default_args}/gnu_yes_2_06x02
./run_benchmark.py --hyperthreading    --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=6 ${default_args}/gnu_yes_2_12x01

./run_benchmark.py --no-hyperthreading --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=1 ${default_args}/gnu_no_2_01x06
./run_benchmark.py --no-hyperthreading --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=2 ${default_args}/gnu_no_2_02x03
./run_benchmark.py --no-hyperthreading --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=3 ${default_args}/gnu_no_2_03x02
./run_benchmark.py --no-hyperthreading --threads_per_rank=2 --nodes_per_tile_side=1 --rank_layout=4 ${default_args}/gnu_no_2_06x01
./run_benchmark.py --hyperthreading    --threads_per_rank=4 --nodes_per_tile_side=1 --rank_layout=1 ${default_args}/gnu_yes_4_01x06
./run_benchmark.py --hyperthreading    --threads_per_rank=4 --nodes_per_tile_side=1 --rank_layout=2 ${default_args}/gnu_yes_4_02x03
./run_benchmark.py --hyperthreading    --threads_per_rank=4 --nodes_per_tile_side=1 --rank_layout=3 ${default_args}/gnu_yes_4_03x02
./run_benchmark.py --hyperthreading    --threads_per_rank=4 --nodes_per_tile_side=1 --rank_layout=4 ${default_args}/gnu_yes_4_06x01

./run_benchmark.py --no-hyperthreading --threads_per_rank=4 --nodes_per_tile_side=1 --rank_layout=1 ${default_args}/gnu_no_4_01x03
./run_benchmark.py --no-hyperthreading --threads_per_rank=4 --nodes_per_tile_side=1 --rank_layout=2 ${default_args}/gnu_no_4_03x01
./run_benchmark.py --hyperthreading    --threads_per_rank=8 --nodes_per_tile_side=1 --rank_layout=1 ${default_args}/gnu_yes_8_01x03
./run_benchmark.py --hyperthreading    --threads_per_rank=8 --nodes_per_tile_side=1 --rank_layout=2 ${default_args}/gnu_yes_8_03x01

