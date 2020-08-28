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


# Run a c12 regression test for the GNU-9 Docker image
export FV3_CONTAINER=fv3gfs-compiled:gnu9_mpich314_nocuda
gsutil copy gs://vcm-ml-public/jenkins-tmp/fv3gfs-compiled_gnu9_mpich314_nocuda.tar.gz .
gunzip fv3gfs-compiled_gnu9_mpich314_nocuda.tar.gz
sarus load ./fv3gfs-compiled_gnu9_mpich314_nocuda.tar ${FV3_CONTAINER}
module unload sarus

sbatch --wait job_jenkins_sarus
md5sum -c ../../tests/pytest/reference/circleci/default/md5.txt
rm *.nc 
rm RESTART/*.nc

# Run a c12 regression test for the GNU-8 Docker image
export FV3_CONTAINER=fv3gfs-compiled:gnu8_mpich314_cuda101
gsutil copy gs://vcm-ml-public/jenkins-tmp/fv3gfs-compiled_gnu8_mpich314_cuda101.tar.gz .
gunzip fv3gfs-compiled_gnu8_mpich314_cuda101.tar.gz
module load sarus
sarus load ./fv3gfs-compiled_gnu8_mpich314_cuda101.tar ${FV3_CONTAINER}
module unload sarus

sbatch --wait job_jenkins_sarus
md5sum -c ../../tests/pytest/reference/circleci/default/md5.txt
