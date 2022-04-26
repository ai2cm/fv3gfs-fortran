#!/bin/bash -f

# This script provides an Jenkins action to run FV3GFS with serialization
# activated in order to generate data for regression tests that are used
# by fv3core. The data is generated and pushed to the GCP storage bucket.

# 2021/01/22 Oliver Fuhrer, Vulcan Inc, oliverf@vulcan.com

# stop on all errors (also on errors in a pipe-redirection)
set -e
set -o pipefail

# the following environment variables need to be set
#   EXPERIMENT_PATTERN - Regex pattern to select the *.yml files in the directory
#                        /tests/serialized_test_data_generation/configs
#   FORTRAN_VERSION    - Can be used to override the FORTRAN_VERSION number in the Makefile
#   VALIDATE_ONLY      - Set to "true" if you only want to compare against data already on GCP
#   FORCE_PUSH         - Set to "true" if you want to overwrite data already on GCP

##################################################
# functions
##################################################

exitError()
{
    echo "ERROR $1: $3" 1>&2
    echo "ERROR     LOCATION=$0" 1>&2
    echo "ERROR     LINE=$2" 1>&2
    exit $1
}

showUsage()
{
    echo "usage: `basename $0` [-h]"
    echo ""
    echo "optional arguments:"
    echo "-h           show this help message and exit"
}

parseOptions()
{
    # process command line options
    while getopts "h" opt
    do
        case $opt in
        h) showUsage; exit 0 ;;
        \?) showUsage; exitError 301 ${LINENO} "invalid command line option (-${OPTARG})" ;;
        :) showUsage; exitError 302 ${LINENO} "command line option (-${OPTARG}) requires argument" ;;
        esac
    done

}

# echo basic setup
echo "####### executing: $0 $* (PID=$$ HOST=$HOSTNAME TIME=`date '+%D %H:%M:%S'`)"

# start timer
T="$(date +%s)"

# parse command line options (pass all of them to function)
parseOptions $*

# echo parameters
echo ""
echo "=== the following setup is being used ==="
echo "BRANCH:          ${BRANCH}"
if [ -n "${EXPERIMENT_PATTERN}" ] ; then
    echo "EXPERIMENT_PATTERN:  ${EXPERIMENT_PATTERN}"
else
    echo "EXPERIMENT_PATTERN: (default)"
    unset EXPERIMENT_PATTERN
fi
if [ -n "${FORTRAN_VERSION}" ] ; then
    echo "FORTRAN_VERSION: ${FORTRAN_VERSION}"
else
    echo "FORTRAN_VERSION: (default)"
    unset FORTRAN_VERSION
fi
echo "VALIDATE_ONLY:   ${VALIDATE_ONLY}"
echo "FORCE_PUSH:      ${FORCE_PUSH}"
echo "========================================="

# set up virtual env, if not already set up
echo ""
echo "========================================="
echo "> Running pip install -r requirements.txt in venv"
python3 -m venv venv
. ./venv/bin/activate
pip install --upgrade pip setuptools wheel
pip install -r requirements.txt

# configure Docker builds and pull dependencies
echo ""
echo "========================================="
echo "> pulling Docker dependencies"
export CUDA=y
export DOCKER_BUILDKIT=1
export BUILD_ARGS="-q"
export BUILD_FROM_INTERMEDIATE=y
export BUILDKIT_PROGRESS=plain
export CALLPYFORT=
make pull_deps

# do the work
echo ""
echo "========================================="
echo "> Running ./make_all_datasets.sh in ./tests/serialize_test_data_generation"
export FSSPEC_GS_REQUESTER_PAYS=vcm-ml
cd tests/serialized_test_data_generation
./make_all_datasets.sh
cd -

# deactivate virtual env
deactivate

# end timer and report time taken
T="$(($(date +%s)-T))"
echo ""
printf "####### time taken: %02d:%02d:%02d:%02d\n" "$((T/86400))" "$((T/3600%24))" "$((T/60%60))" "$((T%60))"

# no errors encountered
echo "####### finished: $0 $* (PID=$$ HOST=$HOSTNAME TIME=`date '+%D %H:%M:%S'`)"
exit 0

# so long, Earthling!
