#!/bin/bash

set -x
if [ "$#" -lt 2 ]; then
    echo "ERROR: You must supply a docker image name and a run directory."
    exit 1
fi
all_args=("$@")
docker_image_name=$1
rundir_host=$2
fv3config_cache_dir=$3

cp submit_job.sh $rundir_host

if [ ! -d $rundir_host  ]; then
    echo "ERROR: provided run directory $2 does not exist"
    exit 1
fi

rundir_container=/rundir

echo $remaining_args
if [ "$#" -gt 2 ]; then # mount fv3config cache directory
    remaining_args=("${all_args[@]:3}");\
    docker run \
        --rm \
        -v $rundir_host:$rundir_container \
        -v $fv3config_cache_dir:$fv3config_cache_dir \
        -it $1 ${remaining_args[@]}
else # the rundir has real input files, not symlinked to a fv3config_cache dir
   
    docker run \
        -d \
        --rm \
        -v $rundir_host:$rundir_container \
        -it $1 
fi
