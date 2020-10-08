## build_docker_images.sh
##
## Bash script that builds two fv3gfs-fortran Docker images: one without and
## and one with Serialization support enabled.  By default, the Docker image
## is sped up by using prebuilt images for the MPI, FMS, ESMF and Serialization
## libraries. (This behaviour can be overridden by setting BUILD_FROM_INTERMEDIATE
## to n).
##
## User is required to enter a valid tag as an input argument.  This tag will
## be appended to the two Docker images built. The Docker image with Serialization
## support enabled will have '-serialize' appended to the final image name.
##
## Each newly build Docker image is pushed to the VCM Google Container Registry.
## A compressed archive of each new Docker image is created and stored in
## Google Storage 
##
##  Mark Cheeseman, VCM
##  October 8, 2020


#!/bin/bash
set -e
set -x

# Read in tag name used for the Docker images to be tested
tagname=$1
if [ -z "${tagname}" ] ; then
  echo "Error: must supply a valid tagname to $0."
  exit 1
fi
<<<<<<< HEAD

=======
>>>>>>> c85b0deb8144beb33c72d25b8995fd8c667978d4
# Set variable to allow parallel building in the Docker image creation
export DOCKER_BUILDKIT=1
export BUILDKIT_PROGRESS=plain

# Speed-up the compilations by using pre-built MPI, FMS, and ESMF images
export BUILD_FROM_INTERMEDIATE=y

# Build FV3 without and with Serialbox support enabled
make pull_deps
make DEP_TAG_NAME=$tagname build build_serialize 

# For each newly built Docker image:
#   - push image to VCM's Google Container Repository (necessary?)
#   - create a tar archive of the image
#   - store tar archive in a Google Storage Bucket
declare -a tags=("$tagname" "${tagname}-serialize")
for tag in ${tags}; do
    container=us.gcr.io/vcm-ml/fv3gfs-compiled:${tag}
    tar_file=fv3gfs-compiled-${tag}.tar
    docker push $container 
    docker save $container -o $tar_file
    gzip $tar_file
    gsutil copy ${tar_file}.gz gs://vcm-jenkins/${tar_file}.gz
done
