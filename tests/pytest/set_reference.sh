#!/bin/bash

# utility script to generate the md5 hashes used to ensure that the model
# still gives bit-identical results.

# Note: You must have run `make test` in the top-level directory before being
#       able to generate new references.

# Uage ./set_reference.sh <image tag> <reference dir>
#   <image tag>      Tag of the Docker image to generate md5 sums (e.g. latest, latest-serialize)
#   <reference dir>  Directory to store the md5 sums

IMG_TAG=$1
if [ -z "${IMG_TAG}" ] ; then
  echo "Error: You must specify an image tag as the first argument"
  exit 1
fi

REF_DIR=$2
if [ -z "${REF_DIR}" ] ; then
  echo "Error: You must specify a directory to store the md5 sums as the second argument"
fi

CWD=$(pwd)

if ! ls $CWD/output/$IMG_TAG/* &>/dev/null ; then
  echo "Error: No input directories found in $CWD/output/$IMG_TAG"
  exit 1
fi

for dir in $CWD/output/$IMG_TAG/* ; do
    run_name=$(basename ${dir})
    mkdir -p $REF_DIR/$run_name
    echo "Processing $dir, storing md5 sums in $REF_DIR/$run_name"
    cd $dir 
    md5sum *.nc RESTART/*.nc > $REF_DIR/$run_name/md5.txt
    if ls test_data/Gen*.dat test_data/*.json >/dev/null 2>&1; then
        md5sum test_data/Gen*.dat test_data/*.json > $REF_DIR/$run_name/md5_serialize.txt
    fi
    cd -
done

exit 0
