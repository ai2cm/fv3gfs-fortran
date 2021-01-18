#!/usr/bin/env python3

import math
import os
import yaml
import click
import fv3config

# constants
DAINT_SECTION = 'hybrid' # one of hybrid or multicore
FV3_NUMBER_OF_TILES = 6

# Piz Daint specification
if DAINT_SECTION == 'hybrid':
    SOCKETS_PER_NODE = 1
    CORES_PER_SOCKET = 12
    HAS_HYPERTHREADING = True
if DAINT_SECTION == 'multicore':
    SOCKETS_PER_NODE = 2
    CORES_PER_SOCKET = 18
    HAS_HYPERTHREADING = True

@click.command()
@click.option('--nodes_per_tile_side', default=1, help='number of compute nodes per tile side')
@click.option('--threads_per_rank', default=1, help='number of OpenMP threads per rank')
@click.option('--hyperthreading/--no-hyperthreading', default=False, help='switch hyperthreading on or off')
@click.option('--executable', default='./fv3.exe', help='path to the FV3GFS executable')
@click.option('--rank_layout', default=1, help='Choose which rank layout to use (1, 2, 3, ...)')
@click.option('--blocksize', default=-1, help='block size to use for physics')
@click.option('--partition', default='normal', help='parittion to submit job to')
@click.argument('yaml_file', type=click.File('r'), nargs=1)
@click.argument('run_directory', type=str, nargs=1)
def run_benchmark(nodes_per_tile_side, threads_per_rank, hyperthreading, executable, 
    rank_layout, blocksize, partition, yaml_file, run_directory):

    # derived variables for Piz Daint
    if hyperthreading:
        assert HAS_HYPERTHREADING
        threads_per_core = 2
    else:
        threads_per_core = 1
    max_cores_per_node = SOCKETS_PER_NODE * CORES_PER_SOCKET
    max_threads_per_node = max_cores_per_node * threads_per_core

    # define SLURM parameters
    slurm_nodes = FV3_NUMBER_OF_TILES * nodes_per_tile_side**2
    slurm_ntasks_per_node = max_threads_per_node // threads_per_rank
    assert threads_per_rank * slurm_ntasks_per_node == max_threads_per_node
    slurm_ntasks_per_core = threads_per_core
    slurm_cpus_per_task = threads_per_rank
    if hyperthreading:
        slurm_hint="multithread"
    else:
        slurm_hint="nomultithread"
    if DAINT_SECTION == 'hybrid':
        slurm_constraint = 'gpu'
    if DAINT_SECTION == 'multicore':
        slurm_constraint = 'mc'

    # SLURM job
    slurm_job = f"""
    #!/bin/bash -l
    #SBATCH --job-name="fv3_bench"
    #SBATCH --account="c1053"
    #SBATCH --time=00:30:00
    #SBATCH --nodes={slurm_nodes}
    #SBATCH --ntasks-per-core={slurm_ntasks_per_core}
    #SBATCH --ntasks-per-node={slurm_ntasks_per_node}
    #SBATCH --cpus-per-task={slurm_cpus_per_task}
    #SBATCH --partition={partition}
    #SBATCH --constraint={slurm_constraint}
    #SBATCH --hint={slurm_hint}

    export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
    export CRAY_CUDA_MPS=1

    t_start=$(date +%s)
    srun {executable}
    t_end=$(date +%s)
    t_elapsed=$(($t_end - $t_start))
    echo "Elapsed[s]=$(t_elapsed)"
    """

    # define MPI decomposition
    ranks_per_tile = slurm_ntasks_per_node * nodes_per_tile_side**2
    max_ranks_in_x = int(math.sqrt(ranks_per_tile))
    rank_layouts = []
    for ranks_in_x in reversed(range(1, max_ranks_in_x + 1)):
        if ranks_per_tile % ranks_in_x == 0:
            ranks_in_y = ranks_per_tile // ranks_in_x
            rank_layouts.append([ranks_in_x, ranks_in_y])
    assert rank_layouts, 'Did not find a suitable rank decomposition'

    # load namelist
    config = yaml.safe_load(yaml_file)
    npx = config['namelist']['fv_core_nml']['npx']
    npy = config['namelist']['fv_core_nml']['npx']
    npz = config['namelist']['fv_core_nml']['npz']

    # override benchmark parameters
    config['namelist']['atmos_model_nml']['blocksize'] = blocksize
    config['namelist']['fv_core_nml']['layout'] = rank_layouts[rank_layout]
    config['namelist']['fv_core_nml']['io_layout'] = [1, 1]
    config['namelist']['coupler_nml']['atmos_nthreads'] = threads_per_rank
    config['namelist']['coupler_nml']['use_hyper_threads'] = hyperthreading
    config['namelist']['coupler_nml']['ncores_per_node'] = SOCKETS_PER_NODE * CORES_PER_SOCKET

    # echo some information
    print('')
    print(f'Configuration file:\n {os.path.abspath(yaml_file.name)}\n')
    print(f'Piz Daint resources:\n'
        f' {DAINT_SECTION}, {slurm_nodes} nodes, {slurm_nodes * max_cores_per_node} cores, '
            f'{slurm_nodes * max_threads_per_node} threads\n')
    print(f'FV3 configuration:\n'
        f' {FV3_NUMBER_OF_TILES} tiles of {npx-1} x {npy-1} x {npz} gridpoints\n'
        f' {(npx-1) // rank_layouts[rank_layout][0]} x {(npy-1) // rank_layouts[rank_layout][1]}'
            f' x {npz} gridpoints per tile on {rank_layouts[rank_layout][0]} x {rank_layouts[rank_layout][1]} ranks\n'
            f' {threads_per_rank} thread(s) per rank with {threads_per_core} thread(s) per core\n')
    
    # create run directory
    run_directory = os.path.abspath(run_directory)
    print(f'Writing run directory:\n {run_directory}\n')
    fv3config.write_run_directory(config, run_directory)

    job_file = os.path.join(run_directory, 'job')
    print(f'Writing SLURM job file:\n {job_file}\n')
    with open(job_file, 'w') as f:
        f.write(slurm_job)

if __name__ == '__main__':
    run_benchmark()