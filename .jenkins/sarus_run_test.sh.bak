#!/bin/bash
set -e
set -x

module load daint-gpu
module add /project/d107/install/modulefiles/
module load cray-python gcloud

python3 -m venv venv
. ./venv/bin/activate
pip3 install git+https://github.com/VulcanClimateModeling/fv3config.git

cd examples
python create_arbitrary_rundir.py c48_config.yml "./c48_test"
deactivate

cd ${PWD}/c48_test
cp ../../.jenkins/job_jenkins_sarus .

export SCRATCH_DIR=${PWD}
gcloud auth configure-docker

dockerfiles=( $( ls ../../docker/Dockerfile.gnu* ) )
for df in ${dockerfiles[@]}; do
   arch=${df:24}
   export FV3_CONTAINER=fv3gfs-compiled:$arch
   export TAR_FILE=${FV3_CONTAINER/":"/"_"}.tar
   module load sarus
   gsutil copy gs://vcm-ml-public/jenkins-tmp/${TAR_FILE}.gz .
   gunzip ${TAR_FILE}.gz
   sarus load ./${TAR_FILE} ${FV3_CONTAINER}
   module unload sarus
   sbatch --wait job_jenkins_sarus
done
