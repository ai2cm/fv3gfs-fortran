#!/bin/bash
set -e
set -x

tags="hpc hpc-serialize"

# Set up the compute node environment
module load daint-mc
module add /project/s1053/install/modulefiles/
module load cray-python gcloud 
gcloud auth configure-docker

# Set up the working directing for the c12 test
# Using the standard python virtual environment for Piz Daint
# defined in https://github.com/VulcanClimateModeling/daint_venv
. /project/s1053/install/venv/sn_1.0/bin/activate
pip install -r requirements.txt

# Run c12 regression test on each Docker image
declare -a tags=("gnu9-mpich314-nocuda" "gnu9-mpich314-nocuda-serialize")
for tag in ${tags}; do
    # Copy archived version of the Docker image from a Google Storage Bucket
    tar_file=fv3gfs-compiled-${tag}.tar
    gsutil copy gs://vcm-jenkins/${tar_file}.gz .
    gunzip ${tar_file}.gz
    # Load archive Docker image into the local Sarus container registry
    export FV3_CONTAINER=fv3gfs-compiled:${tag}
    module load sarus
    sarus load ./${tar_file} ${FV3_CONTAINER}
    module unload sarus
done

# Launch SLURM job
pytest --image_runner=sarus --image=fv3gfs-compiled --image_version=hpc --refdir=$(pwd)/tests/pytest/reference/circleci --maxfail=1 tests/pytest
