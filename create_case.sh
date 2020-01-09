#!/bin/bash

## Input parsing

function print_help(){
    echo "Usage:"
    echo "    create_case.sh [-f] <experiment_name>"
    exit 1
}

create_new_rundir=false

while getopts ":f" opt
do
    case "$opt" in
        f)
            create_new_rundir=true
            ;;
        *)
            print_help
            exit 1
            ;;
    esac
done
shift $(( OPTIND - 1 ))

if [ "$#" -ne 1 ]; then
    print_help
fi

## REAL CODE
experiment_name=$1
inputdata=inputdata/fv3gfs-data-docker
rundir=experiments/$experiment_name/rundir

# check if rundir already exists for specified experiment
if [ -e "$rundir" ] && [ $create_new_rundir == false ] ; then
    echo "That experiment already exists. Pass -f flag to overwrite "
    exit 1
fi

# create new experiment or overwrite if already exists
rm -r -f $rundir
mkdir -p experiments/$experiment_name
cp -r $inputdata/rundir $rundir
cp submit_job.sh $rundir
