#!/bin/bash
set -e
set -x

# Set environment on Piz Daint 
module load daint-gpu
module add /project/d107/install/modulefiles/
module load cray-python gcloud

# Create working directory using a Python virtual environment 
python3 -m venv venv
. ./venv/bin/activate
pip3 install "git+https://github.com/VulcanClimateModeling/fv3config.git"

# Setup the working directory for a C48 FV3GFS run
cd examples
cp /project/d107/mcheese/yaml_files/fv3config_c48.yml ./c48_config.yml
python create_arbitrary_rundir.py c48_config.yml "./c48_test"
deactivate

# Copy over a SLURM job submission script to run the c48 test run
cd ${PWD}/c48_test
cp /project/d107/mcheese/jobscripts/job_jenkins_sarus .

# SCRATCH_DIR environment variable is required for the SLURM job to run
#export SCRATCH_DIR=/scratch/snx3000/olifu/jenkins/workspace/fv3gfs/examples/rundir
export SCRATCH_DIR=${PWD}
gcloud auth configure-docker

# Get the dockerfiles for the images to be tested on Piz Daint
dockerfiles=( $( ls ../../docker/Dockerfile.gnu* ) )

for df in ${dockerfiles[@]}; do

   # Grab the architecture tag from the dockerfile filename
   arch=${df:24}
   export FV3_CONTAINER=fv3gfs-compiled:$arch
   export TAR_FILE=${FV3_CONTAINER/":"/"_"}.tar

   # Download archived Docker image and load it into the local Sarus Docker image repository
   module load sarus
   gsutil copy gs://vcm-ml-public/jenkins-tmp/${TAR_FILE} .
   sarus load ./${TAR_FILE} ${FV3_CONTAINER}
   module unload sarus

   # Launch SLURM job.  Wait until job finishes before proceeding.
   sbatch --wait job_jenkins_sarus
   rm fv3gfs-fortran.sif
done
