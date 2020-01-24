#!/bin/bash


if [ "$#" -lt 2 ]; then
    echo "ERROR: You must supply a docker image name and a run directory."
    exit 1
fi

docker_image_name=$1
rundir_host=$2
fv3config_cache_dir=$3

cp submit_job.sh $rundir_host

if [ ! -d $rundir_host  ]; then
    echo "ERROR: provided run directory $2 does not exist"
    exit 1
fi

rundir_container=/rundir

if [ "$#" -gt 2 ]; then # mount fv3config cache directory
    docker run \
        --rm \
        -v $rundir_host:$rundir_container \
        -v $fv3config_cache_dir:$fv3config_cache_dir \
        -it $1
else # run container with no command argument
    docker run \
        -d \
        --rm \
        -v $rundir_host:$rundir_container \
        -it $1
fi
