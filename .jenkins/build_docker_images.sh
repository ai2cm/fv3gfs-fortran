#!/bin/bash

# Set variable to allow parallel building in the Docker image creation
export DOCKER_BUILDKIT=1
export BUILDKIT_PROGRESS=plain
targets="build build_serialize_gt4py_dev"
tags="gnu9-mpich314-nocuda gnu9-mpich314-nocuda-serialize"

# Speed-up the compilations by using pre-built MPI, FMS, and ESMF images
export BUILD_FROM_INTERMEDIATE=y

# We build and test Docker images without CUDA support
export CUDA=n

# Build FV3 without and with Serialbox support enabled
make pull_deps
make ${targets} 

# For each newly built Docker image:
#   - push image to VCM's Google Container Repository (necessary?)
#   - create a tar archive of the image
#   - store tar archive in a Google Storage Bucket
for tag in ${tags} ; do
    container=us.gcr.io/vcm-ml/fv3gfs-compiled:${tag}
    tar_file=fv3gfs-compiled-${tag}.tar
    docker push $container 
    docker save $container -o $tar_file
    gzip $tar_file
    gsutil copy ${tar_file}.gz gs://vcm-jenkins/${tar_file}.gz
done
