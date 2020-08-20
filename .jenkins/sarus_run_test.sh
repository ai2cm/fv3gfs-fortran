#!/bin/bash
set -e
set -x

# Set up the compute node environment
module load daint-gpu
module add /project/d107/install/modulefiles/
module load cray-python gcloud sarus
gcloud auth configure-docker


# Set up the working directing for the c12 test
cd examples
. /project/d107/install/venv/sn_1.0/bin/activate
python write_rundir.py ../tests/pytest/config/default.yml "./c12_test"
deactivate

cd ${PWD}/c12_test
cp ../../.jenkins/job_jenkins_sarus .
export SCRATCH_DIR=${PWD}


# Import the Docker images to be tested on Daint
dockerfiles=( $( ls ../../docker/Dockerfile.* ) )
for df in ${dockerfiles[@]}; do
   arch=${df:24}
   export FV3_CONTAINER=fv3gfs-compiled:$arch
   export TAR_FILE=${FV3_CONTAINER/":"/"_"}.tar
   gsutil copy gs://vcm-jenkins/${TAR_FILE}.gz .
   gunzip ${TAR_FILE}.gz
   sarus load ./${TAR_FILE} ${FV3_CONTAINER}
done
module unload sarus


# Perform the c12 test for each imported Docker image
for df in ${dockerfiles[@]}; do
   export FV3_CONTAINER=fv3gfs-compiled:${df:24}
   sbatch --wait job_jenkins_sarus
done
