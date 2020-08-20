#!/bin/bash

# Build the non-serialize and serialize versions of a Docker image
make build build_serialize

# Push images to VCM's Google Container Repository
container=us.gcr.io/vcm-ml/fv3gfs-compiled:gnu_openmpi
tar_file_prefix=fv3gfs-compiled-gnu_openmpi
docker push ${container}
docker push ${container}-serialize

# Create an archived version of each Docker image
docker save ${container} -o ${tar_file_prefix}.tar
gzip ${tar_file_prefix}.tar
docker save ${container}-serialize -o ${tar_file_prefix}-serialize.tar
gzip ${tar_file_prefix}-serialize.tar

# Copy the archives to a public Google Storage Bucket
gsutil copy ${tar_file_prefix}.tar.gz gs://vcm-ml-public/jenkins-tmp/${tar_file_prefix}.tar.gz
gsutil copy ${tar_file_prefix}-serialize.tar.gz gs://vcm-ml-public/jenkins-tmp/${tar_file_prefix}-serialize.tar.gz
