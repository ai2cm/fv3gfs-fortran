#!/bin/bash
## sarus_run_test.sh
##
## Bash script that runs a C12 reference test using two Docker images (one with
## Serialization support and one without).  
##
## Sarus cannot pull Docker images directly from the Google Container Registry.
## The work-around is to download compressed archive files of the desired
## Docker images from Google Storage, uncompress the archives and add them in
## a local Docker repo on Daint.  The Docker images can then be run as normal
## via Sarus.
##
##   Mark Cheeseman, VCM 
##   October 8, 2020
##
##
## C12 runs and MD5 checksum verification is performed under pytest.  The
## user needs to tell pytest that Sarus will be used for Docker image 
## deployment via the --image_runner option.
##
##    Jeremy McGibbon, VCM
##    October 8, 2020

set -e
set -x

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
module load sarus

tar_file=fv3gfs-compiled-gnu9-mpich314-nocuda.tar
gsutil copy gs://vcm-jenkins/${tar_file}.gz .
gunzip ${tar_file}.gz
export FV3_CONTAINER=fv3gfs-compiled:gnu9-mpich314-nocuda
sarus load ./${tar_file} ${FV3_CONTAINER}

tar_file=fv3gfs-compiled-gnu9-mpich314-nocuda-serialize.tar
gsutil copy gs://vcm-jenkins/${tar_file}.gz .
gunzip ${tar_file}.gz
export FV3_CONTAINER=fv3gfs-compiled:gnu9-mpich314-nocuda-serialize
sarus load ./${tar_file} ${FV3_CONTAINER}

module unload sarus

# Launch SLURM job
pytest --image_runner=sarus --image=fv3gfs-compiled --image_version=gnu9-mpich314-nocuda --refdir=$(pwd)/tests/pytest/reference/circleci --maxfail=1 tests/pytest
