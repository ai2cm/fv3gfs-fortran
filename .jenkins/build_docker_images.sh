#!/bin/bash
## build_docker_images.sh
##
## Bash script that builds two fv3gfs-fortran Docker images: one without and
## and one with Serialization support enabled.  By default, the Docker image
## is sped up by using prebuilt images for the MPI, FMS, ESMF and Serialization
## libraries. (This behaviour can be overridden by setting BUILD_FROM_INTERMEDIATE
## to n).
##
## Each newly build Docker image is pushed to the VCM Google Container Registry.
## A compressed archive of each new Docker image is created and stored in
## Google Storage 
##
##  Mark Cheeseman, VCM
##  October 8, 2020


set -e
set -x

# Set variable to allow parallel building in the Docker image creation
export DOCKER_BUILDKIT=1
export BUILDKIT_PROGRESS=plain

# Speed-up the compilations by using pre-built MPI, FMS, and ESMF images
export BUILD_FROM_INTERMEDIATE=y

# Build FV3 without and with Serialbox support enabled
make pull_deps
make build build_serialize 

# For each newly built Docker image:
#   - push image to VCM's Google Container Repository (necessary?)
#   - create a tar archive of the image
#   - store tar archive in a Google Storage Bucket
container=us.gcr.io/vcm-ml/fv3gfs-compiled:gnu9-mpich314-nocuda
tar_file=fv3gfs-compiled-gnu9-mpich314-nocuda.tar
docker push $container
docker save $container -o $tar_file
gzip $tar_file
gsutil copy ${tar_file}.gz gs://vcm-jenkins/${tar_file}.gz

container=us.gcr.io/vcm-ml/fv3gfs-compiled:gnu9-mpich314-nocuda-serialize
tar_file=fv3gfs-compiled-gnu9-mpich314-nocuda-serialize.tar
docker push $container
docker save $container -o $tar_file
gzip $tar_file
gsutil copy ${tar_file}.gz gs://vcm-jenkins/${tar_file}.gz

