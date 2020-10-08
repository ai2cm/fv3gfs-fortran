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
## User is required to enter a valid tag as an input argument.  This tag will
## be appended to the two Docker images used for the C12 reference tests. The 
## Docker image with Serialization support enabled will have '-serialize' 
## appended to its image name.
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

#!/bin/bash
set -e
set -x

# Read in tag name used for the Docker images to be tested
tagname=$1
if [ -z "${tagname}" ] ; then
  echo "Error: must supply a valid tagname to $0."
  exit 1
fi

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
declare -a tags=("$tagname" "${tagname}-serialize")
for tag in ${tags}; do
    # Copy archived version of the Docker image from a Google Storage Bucket
    tar_file=fv3gfs-compiled-${tag}.tar
    gsutil copy gs://vcm-jenkins/${tar_file}.gz .
    gunzip ${tar_file}.gz
    # Load archive Docker image into the local Sarus container registry
    export FV3_CONTAINER=fv3gfs-compiled:${tag}
    sarus load ./${tar_file} ${FV3_CONTAINER}
done
module unload sarus

# Launch SLURM job
pytest --image_runner=sarus --image=fv3gfs-compiled --image_version=${tagname} --refdir=$(pwd)/tests/pytest/reference/circleci --maxfail=1 tests/pytest
