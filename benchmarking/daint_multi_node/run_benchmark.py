#!/usr/bin/env python3

# This script generates a rundirectory given user-provided arguments and a configuration
# file. It relies on fv3config for setting up the run directory. Once done, a SLURM
# script is generated and submitted. Optionally, we wait for the completion of the SLURM
# job.

# 2021/01/22 Oliver Fuhrer, Vulcan Inc, oliverf@vulcan.com

import math
import os
import sys
import shutil
import hashlib
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
@click.option('--blocksize', default=-1, help='block size to use for physics')
@click.option('--executable', default='./fv3.exe', help='path to the FV3GFS executable')
@click.option('--force/--no-force', default=False, help='overwrite existing run directory (if it exists)')
@click.option('--hyperthreading/--no-hyperthreading', default=False, help='switch hyperthreading on or off')
@click.option('--module_env', default=None, help='file to source to pre-load modules')
@click.option('--nodes_per_tile_side', default=1, help='number of compute nodes per tile side')
@click.option('--partition', default='normal', help='parittion to submit job to')
@click.option('--rank_layout', default=-1, help='Choose which rank layout to use (1, 2, 3, ...)')
@click.option('--threads_per_rank', default=1, help='number of OpenMP threads per rank')
@click.option('--timesteps', default=60, help='number of timesteps')
@click.option('--wait/--no-wait', default=True, help='wait for SLURM job to complete')
@click.argument('yaml_file', type=click.File('r'), nargs=1)
@click.argument('run_directory', type=str, nargs=1)
def run_benchmark(nodes_per_tile_side, threads_per_rank, hyperthreading, executable, 
    rank_layout, blocksize, partition, force, timesteps, wait, module_env, yaml_file, run_directory):

    # echo command
    print('\nRunning command:')
    print('', ' '.join(sys.argv))

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

    # define MPI decomposition
    ranks_per_tile = slurm_ntasks_per_node * nodes_per_tile_side**2
    max_ranks_in_x = int(math.sqrt(ranks_per_tile))
    rank_layouts = []
    for ranks_in_x in range(1, ranks_per_tile+1):
        if ranks_per_tile % ranks_in_x == 0:
            ranks_in_y = ranks_per_tile // ranks_in_x
            rank_layouts.append([ranks_in_x, ranks_in_y])
            if ranks_in_x <= max_ranks_in_x:
                default_layout = len(rank_layouts)
    assert rank_layouts, 'Did not find a suitable rank decomposition'
    if rank_layout == -1:
        rank_layout = default_layout

    # load namelist
    config = yaml.safe_load(yaml_file)
    npx = (config['namelist']['fv_core_nml']['npx']-1)*nodes_per_tile_side + 1
    config['namelist']['fv_core_nml']['npx'] = npx
    npy = (config['namelist']['fv_core_nml']['npy']-1)*nodes_per_tile_side + 1
    config['namelist']['fv_core_nml']['npy'] = npy
    npz = config['namelist']['fv_core_nml']['npz']
    dt = config['namelist']['coupler_nml']['dt_atmos']
    if npx > 3073:
        dt = math.floor(3072./(npx-1)*dt)
        while 3600 % dt:
            dt -= 1
        assert dt > 0
    config['namelist']['coupler_nml']['dt_atmos'] = dt
    config['namelist']['coupler_nml']['dt_ocean'] = dt

    # override benchmark parameters
    config['namelist']['atmos_model_nml']['blocksize'] = blocksize
    config['namelist']['fv_core_nml']['layout'] = rank_layouts[rank_layout-1]
    config['namelist']['fv_core_nml']['io_layout'] = [1, 1]
    config['namelist']['coupler_nml']['atmos_nthreads'] = threads_per_rank
    config['namelist']['coupler_nml']['use_hyper_thread'] = hyperthreading
    config['namelist']['coupler_nml']['ncores_per_node'] = SOCKETS_PER_NODE * CORES_PER_SOCKET
    config['namelist']['coupler_nml']['months'] = 0
    config['namelist']['coupler_nml']['days'] = 0
    config['namelist']['coupler_nml']['hours'] = 0
    config['namelist']['coupler_nml']['minutes'] = 0
    config['namelist']['coupler_nml']['seconds'] = dt * timesteps

    # echo some information
    print('')
    print(f'Configuration file:\n {os.path.abspath(yaml_file.name)}\n')
    print(f'Piz Daint resources:\n'
        f' {DAINT_SECTION}, {slurm_nodes} nodes, {slurm_nodes * max_cores_per_node} cores, '
            f'{slurm_nodes * max_threads_per_node} threads\n')
    print(f'FV3 configuration:\n'
        f' {FV3_NUMBER_OF_TILES} tiles of {npx-1} x {npy-1} x {npz} gridpoints\n'
        f' {(npx-1) // rank_layouts[rank_layout-1][0]} x {(npy-1) // rank_layouts[rank_layout-1][1]}'
            f' x {npz} gridpoints per rank on {rank_layouts[rank_layout-1][0]} x {rank_layouts[rank_layout-1][1]} ranks per tile'
            f' (rank layout {rank_layout} of {len(rank_layouts)})\n'
        f' {threads_per_rank} thread(s) per rank with {threads_per_core} thread(s) per core\n')
    
    # create run directory
    run_directory = os.path.abspath(run_directory)
    if os.path.isdir(run_directory):
        if force:
            shutil.rmtree(run_directory)
        else:
            assert False, f'Run directory ({run_directory}) already exists (use --force to overwrite)'
    print(f'Writing run directory:\n {run_directory}\n')
    fv3config.write_run_directory(config, run_directory)

    # copy executable and some meta-information
    print(f'Copying executable:\n {executable}')
    shutil.copy2(executable, os.path.join(run_directory, 'fv3.exe'))
    md5_hash = hashlib.md5(open(executable,'rb').read()).hexdigest()
    print(f' md5 hash is {md5_hash}\n')
    if module_env is not None:
        shutil.copy2(module_env, os.path.join(run_directory, 'module.env'))
    shutil.copy2(yaml_file.name, os.path.join(run_directory, 'config.yml'))

    # create SLURM job file
    slurm_job = f"""#!/bin/bash -l
#SBATCH --job-name="fv3_bench"
#SBATCH --account="s1053"
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
export OMP_STACKSIZE=32M

if [ -f ./module.env ] ; then
  source ./module.env
fi

t_start=$(date +%s)
srun ./fv3.exe
t_end=$(date +%s)
t_elapsed=$(($t_end - $t_start))
echo "Elapsed[s]=${{t_elapsed}}"

exit 0
"""
    job_file = os.path.join(run_directory, 'job')
    print(f'Writing SLURM job file:\n {job_file}\n')
    with open(job_file, 'w') as f:
        f.write(slurm_job)

    # run SLURM job
    print(f'Running SLURM job:')
    os.system(f'cd {run_directory}; sbatch {"--wait" if wait else ""} job')

if __name__ == '__main__':
    run_benchmark()
