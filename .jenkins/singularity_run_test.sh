#!/bin/bash
set -e
set -x

module load daint-gpu
module add /project/d107/install/modulefiles/
module load cray-python gcloud

python3 -m venv venv
. ./venv/bin/activate
pip3 install git+https://github.com/VulcanClimateModeling/fv3config.git@feature/more_hpc_examples#egg=fv3config

cd examples
python create_arbitrary_rundir.py c48_config.yml "./c48_test"
deactivate

cd ${PWD}/c48_test
cp ../../.jenkins/job_jenkins_singularity .

export SCRATCH_DIR=${PWD}
gcloud auth configure-docker

dockerfiles=( $( ls ../../docker/Dockerfile.gnu* ) )
for df in ${dockerfiles[@]}; do
   arch=${df:24}
   export FV3_CONTAINER=fv3gfs-compiled:$arch
   module load singularity
   singularity pull fv3gfs-fortran.sif docker://us.gcr.io/vcm-ml/${FV3_CONTAINER}
   module unload singularity
   sbatch --wait job_jenkins_singularity
   rm fv3gfs-fortran.sif
done
