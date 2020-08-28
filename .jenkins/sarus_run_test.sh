#!/bin/bash
set -e
set -x

# Set up the compute node environment
module load daint-gpu
module add /project/d107/install/modulefiles/
module load cray-python gcloud sarus
gcloud auth configure-docker


# Set up the working directing for the c12 test
echo "Setting up working directory"
cd examples
. /project/d107/install/venv/sn_1.0/bin/activate
python write_rundir.py ../tests/pytest/config/default.yml "./c12_test"
deactivate

cd ${PWD}/c12_test
cp ../../.jenkins/job_jenkins_sarus .
export SCRATCH_DIR=${PWD}


# Import the Docker image to be tested on Daint
echo "Import GNU-9 compiled Docker image into Sarus"
export FV3_CONTAINER=fv3gfs-compiled:gnu9_mpich314_nocuda
gsutil copy gs://vcm-ml-public/jenkins-tmp/fv3gfs-compiled_gnu9_mpich314_nocuda.tar.gz .
gunzip fv3gfs-compiled_gnu9_mpich314_nocuda.tar.gz
sarus load ./fv3gfs-compiled_gnu9_mpich314_nocuda.tar ${FV3_CONTAINER}
module unload sarus


# run a c12 run for the specified Docker image
echo "Submitting SLURM c12 run"
sbatch --wait job_jenkins_sarus

# verify the output files using a md5sum check
echo "Performing md5sum check of c12 run output"
md5sum -c ../../tests/pytest/reference/circleci/default/md5.txt

