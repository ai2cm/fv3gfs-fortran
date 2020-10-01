#!/bin/bash -f

# stop on all errors
set -e

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

# echo configuration parameters
echo "=== the following setup is being used ==="
echo "BRANCH:          ${BRANCH}"
if [ -n "${CONFIG_PATTERN}" ] ; then
    echo "CONFIG_PATTERN:  ${CONFIG_PATTERN}"
else
    echo "CONFIG_PATTERN: (default)"
    unset CONFIG_PATTERN
fi
if [ -n "${FORTRAN_VERSION}" ] ; then
    echo "FORTRAN_VERSION: ${FORTRAN_VERSION}"
else
    echo "FORTRAN_VERSION: (default)"
    unset FORTRAN_VERSION
fi
echo "FORCE_PUSH:      ${FORCE_PUSH}"
echo "=== the following setup is being used ==="

# set up virtual env, if not already set up
echo ">> Running pip install -r requirements.txt in venv"
python3 -m venv venv
. ./venv/bin/activate
pip install --upgrade pip setuptools wheel
pip install -r requirements.txt

# configure Docker builds
export DOCKER_BUIDKIT=1
export BUILD_ARGS="-q"

# do the work
echo ">> Running ./make_all_datasets.sh in ./tests/serialize_test_data_generation"
cd tests/serialized_test_data_generation
./make_all_datasets.sh
cd -

# end timer and report time taken
T="$(($(date +%s)-T))"
printf "####### time taken: %02d:%02d:%02d:%02d\n" "$((T/86400))" "$((T/3600%24))" "$((T/60%60))" "$((T%60))"

# no errors encountered
echo "####### finished: $0 $* (PID=$$ HOST=$HOSTNAME TIME=`date '+%D %H:%M:%S'`)"
exit 0

# so long, Earthling!
