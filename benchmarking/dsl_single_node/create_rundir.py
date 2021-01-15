#!/usr/bin/env python3

import fv3config
import yaml

# arguments
yaml_file = 'config/test.yml')
ntiles = 6
number_of_nodes_per_tile_side = 1
number_of_threads_per_rank = 2
hyper_threading = True

# Piz Daint specification
PARTITION='hybrid' # one of hybrid or multicore
if PARTITION == 'hybrid':
    SOCKETS_PER_NODE = 1
    CORES_PER_SOCKET = 12
    HAS_HYPERTHREADING = True
if PARTITION == 'multicore':
    SOCKETS_PER_NODE = 2
    CORES_PER_SOCKET = 18
    HAS_HYPERTHREADING = True

# derived variables for Piz Daint
if hyper_threading:
    assert HAS_HYPERTHREADING
    number_of_threads_per_core = 2
else:
    number_of_threads_per_core = 1
max_number_of_threads_per_node = SOCKETS_PER_NODE * CORES_PER_SOCKET * number_of_threads_per_core

# compute SLURM parameters
slurm_nodes = ntiles * number_of_nodes_per_tile_side * number_of_nodes_per_tile_side
slurm_ntasks_per_node = max_number_of_threads_per_node // number_of_threads_per_rank
assert number_of_threads_per_rank * slurm_ntasks_per_node == max_number_of_threads_per_node
slurm_ntasks_per_core = number_of_threads_per_core
slurm_cpus_per_task = number_of_threads_per_rank
if hyper_threading:
    slurm_hint="multithread"
else:
    slurm_hint="nomultithread"
if PARTITION == 'hybrid':
    slurm_constraint = 'gpu'
if PARTITION == 'multicore':
    slurm_constraint = 'mc'

# compute FV3 parameters
fv3_blocksize = ???
fv3_layout = f"{???},{???}"
fv3_atmos_nthreads = number_of_threads_per_rank
fv3_use_hyper_threads = '.true.' if hyper_threads else '.false.'
fv3_ncores_per_node = SOCKETS_PER_NODE * CORES_PER_SOCKET
fv3_io_layout = '1,1'


config = yaml.safe_load(open(yaml_file, 'r'))

fv3config.write_run_directory(config, 'rundir')




## 6 MPI ranks per socket, 4 OpenMP threads, hyperthreading on
#!/bin/bash -l
#SBATCH --job-name="fv3_bench"
#SBATCH --account="c1053"
#SBATCH --time=01:00:00
#SBATCH --nodes=6
#SBATCH --ntasks-per-core=2
#SBATCH --ntasks-per-node=6
#SBATCH --cpus-per-task=4
#SBATCH --partition=normal
#SBATCH --constraint=gpu
#SBATCH --hint=multithread

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export CRAY_CUDA_MPS=1

srun ./fv3.exe

###  ## 12 MPI ranks per socket, 2 OpenMP threads, hyperthreading on
###  #!/bin/bash -l
###  #SBATCH --ntasks-per-core=2
###  #SBATCH --ntasks-per-node=12
###  #SBATCH --cpus-per-task=2
###  #SBATCH --hint=multithread
###  
###  ## 6 MPI ranks per socket, 2 OpenMP threads, hyperthreading off
###  #SBATCH --ntasks-per-core=1
###  #SBATCH --ntasks-per-node=6
###  #SBATCH --cpus-per-task=2
###  #SBATCH --hint=nomultithread
###  
###  ## 12 MPI ranks per socket, 1 OpenMP thread, hyperthreading off
###  #SBATCH --ntasks-per-core=1
###  #SBATCH --ntasks-per-node=12
###  #SBATCH --cpus-per-task=1
###  #SBATCH --hint=nomultithread

