#!/bin/bash -l
#SBATCH --job-name="fv3.exe"
#SBATCH --account=d107
#SBATCH --mail-type=ALL
#SBATCH --mail-user=oliverf@vulcan.com
#SBATCH --time=00:15:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-core=1
#SBATCH --ntasks-per-node=12
#SBATCH --cpus-per-task=1
#SBATCH --partition=normal
#SBATCH --constraint=gpu
#SBATCH --hint=nomultithread

# be loud and exit upon error
set -x
set -e

# setup environment
export MPICH_ENV_DISPLAY=1
export MPICH_MPIIO_CB_ALIGN=2
export MALLOC_MMAP_MAX_=0
export MALLOC_TRIM_THRESHOLD_=536870912
export NC_BLKSZ=1M
# necessary for OpenMP when using Intel
export KMP_STACKSIZE=256m
export SLURM_CPU_BIND=verbose
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export CRAY_CUDA_MPS=1

# avoid segfaults
ulimit -s unlimited

# run testsuite

source ./setup.sh

${testsuite} ${config} -n 6 --nprocio 0 -v 1 --color -f --tolerance=TOLERANCE_${real_type} --testlist=testlist.xml -o testsuite.out --mpicmd='mpirun --allow-run-as-root  --mca btl_vader_single_copy_mechanism none --oversubscribe -np' ${steps} ${only} ${tune}

source ./teardown.sh

