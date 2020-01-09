#!/bin/bash


if [ "$#" -lt 2 ]; then
    echo "ERROR: You must supply a docker image name and an experiment name."
    exit 1
fi

docker_image_name=$1
experiment_name=$2

if [ ! -d experiments/$experiment_name ]; then
    echo "ERROR: You must create a rundir directory (try create_case.sh $experiment_name)"
    exit 1
fi


rundir_host=$PWD/experiments/$experiment_name/rundir
rundir_container=/rundir
fixdatadir_host=$PWD/inputdata/fv3gfs-data-docker/fix.v201702
fixdatadir_container=/inputdata/fix.v201702
inputdata_host=$PWD/inputdata
inputdata_container=/FV3/inputdata

if [ "$#" -gt 2 ]; then # provide third and later arguments as command to run inside container
    docker run \
        --rm \
        -v $rundir_host:$rundir_container \
        -v $fixdatadir_host:$fixdatadir_container \
        -v $inputdata_host:$inputdata_container \
        -it $1 ${@:3}
else # run container with no command argument
    docker run \
        -d \
        --rm \
        -v $rundir_host:$rundir_container \
        -v $fixdatadir_host:$fixdatadir_container \
        -v $inputdata_host:$inputdata_container \
        -it $1
fi
