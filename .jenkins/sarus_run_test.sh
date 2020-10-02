#!/bin/bash
set -e
set -x

# Set up the compute node environment
module load daint-gpu
module add /project/d107/install/modulefiles/
module load cray-python gcloud 
gcloud auth configure-docker

# Set up the working directing for the c12 test
# Using the standard python virtual environment for Piz Daint
# defined in https://github.com/VulcanClimateModeling/daint_venv
. /project/d107/install/venv/sn_1.0/bin/activate
pip install -r requirements.txt

# Copy archived version of the Docker image from a Google Storage Bucket
tar_file=fv3gfs-compiled-${tag}.tar
gsutil copy gs://vcm-jenkins/${tar_file}.gz .
gunzip ${tar_file}.gz

# Load archive Docker image into the local Sarus container registry
export FV3_CONTAINER=fv3gfs-compiled:${tag}
module load sarus
sarus load ./${tar_file} ${FV3_CONTAINER}
module unload sarus

# Launch SLURM job
pytest --image_runner=sarus --image=fv3gfs-compiled --image_tag=hpc
