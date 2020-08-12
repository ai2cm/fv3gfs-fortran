#!/bin/bash
set -e
set -x

module load daint-gpu
module add /project/d107/install/modulefiles/
module load cray-python gcloud

python3 -m venv venv
. ./venv/bin/activate
pip3 install -r requirements_dev.txt
pip3 install -e .

cd examples
python create_rundir.py c48_config.yml "./c48_test"
deactivate

cd ${PWD}/c48_test
cp /project/d107/mcheese/jobscripts/job_jenkins_sarus .

export SCRATCH_DIR=/scratch/snx3000/olifu/jenkins/workspace/fv3gfs/examples/c48_test
gcloud auth configure-docker

declare -a archs=("gnu8_mpich314_nocuda" "gnu9_mpich314_nocuda" ) 
for arch in ${archs[@]}; do
   export FV3_CONTAINER=fv3gfs-compiled:$arch
   export TAR_FILE=${FV3_CONTAINER/":"/"_"}.tar
   module load sarus
   gsutil copy gs://vcm-ml-public/jenkins-tmp/${TAR_FILE} .
   sarus load ./${TAR_FILE} ${FV3_CONTAINER}
  module unload sarus
   sbatch --wait job_jenkins_sarus
done
