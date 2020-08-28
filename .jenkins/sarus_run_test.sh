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
   gsutil copy gs://vcm-ml-public/jenkins-tmp/${TAR_FILE}.gz .
   gunzip ${TAR_FILE}.gz
   sarus load ./${TAR_FILE} ${FV3_CONTAINER}
done
module unload sarus


# Perform a series of runs 
for df in ${dockerfiles[@]}; do
   # run a c12 run for the specified Docker image
   export FV3_CONTAINER=fv3gfs-compiled:${df:24}
   sbatch --wait job_jenkins_sarus

   # verify the output files using a md5sum check
   md5sum -c ../../tests/pytest/reference/circleci/default/md5.txt

   # clean up the working directory for the next run
   rm *.nc RESTART/*.nc
done


