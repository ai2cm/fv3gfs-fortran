#!/bin/bash

set -x
if [ "$#" -lt 2 ] ; then
    echo "ERROR: You must supply a docker image name and a run directory."
    exit 1
fi
all_args=("$@")
docker_image_name=$1
rundir_host=$2
fv3config_cache_dir=$3

if [ ! -d $rundir_host ] ; then
    echo "ERROR: provided run directory $2 does not exist"
    exit 1
fi
if [ ! -f $rundir_host/submit_job.sh ] ; then
    echo "Missing submit_job.sh in $rundir_host. Abort!"
    exit 1
fi

rundir_container=/rundir

if [ "$#" -gt 2 ] ; then # mount fv3config cache directory
    remaining_args=("${all_args[@]:3}");\
    docker run \
        --rm \
        -v $rundir_host:$rundir_container \
        -v $fv3config_cache_dir:$fv3config_cache_dir \
        -it $1 ${remaining_args[@]}
else # the rundir has real input files, not symlinked to a fv3config_cache dir
    if [ -n "`find $rundir_host -type l -ls`" ] ; then
        echo "Symbolic links found in run directory but no cache directory specified"
        exit 1
    fi
    docker run \
        --rm \
        -v $rundir_host:$rundir_container \
        -it $1 ${remaining_args[@]}
fi

exit 0
