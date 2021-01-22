#!/bin/bash

# stop on all errors
set -e

INSTALL_DIR=${PROJECT}/../install
FV3_EXE_DIR=${INSTALL_DIR}/fv3gfs-fortran/
VENV_DIR=${INSTALL_DIR}/venv/vcm_1.0/

CONFIG_LIST="c128"
FV3_EXE_NAME="fv3_64bit.exe"
TIMESTEPS="61"

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

containsElement()
{
  local e
  for e in "${@:2}"; do [[ "$e" == "$1" ]] && return 0; done
  return 1
}

showUsage()
{
    echo "usage: `basename $0` [-h]"
    echo ""
    echo "optional arguments:"
    echo "-h             show this help message and exit"
    echo "-c <compiler>  compiler to use [gnu|intel]"
}

parseOptions()
{
    # process command line options
    while getopts "hc:" opt
    do
        case $opt in
        h) showUsage; exit 0 ;;
        c) compiler=$OPTARG ;;
        \?) showUsage; exitError 301 ${LINENO} "invalid command line option (-${OPTARG})" ;;
        :) showUsage; exitError 302 ${LINENO} "command line option (-${OPTARG}) requires argument" ;;
        esac
    done
    test -n "${compiler}" || exitError 310 ${LINENO} "Option <compiler> is not set"
    local compilers=(gnu intel)
    containsElement "${compiler}" "${compilers[@]}" || exitError 312 ${LINENO} "Invalid compiler (${compiler}) chosen"
}

# echo basic setup
echo "####### executing: $0 $* (PID=$$ HOST=$HOSTNAME TIME=`date '+%D %H:%M:%S'`)"

# start timer
T="$(date +%s)"

# parse command line options (pass all of them to function)
parseOptions $*

# check presence of env directory
pushd `dirname $0` > /dev/null
envloc=`/bin/pwd`/..
popd > /dev/null

# determine root directory
pushd ${envloc}/.. > /dev/null
rootdir=`/bin/pwd`
popd > /dev/null

# setup module environment and default queue
# note: disable error checking since script uses fails for checks
set +e
. ${envloc}/env/machineEnvironment.sh
set -e

# load scheduler tools
. ${envloc}/env/schedulerTools.sh

# run the benchmarks
# note: this relies on the fact that daint_gnu and daint_intel are the first
#       two options for the configure script
echo "### run benchmarks"

echo "==== module list ===="
module list
echo "====================="

python3 -m venv venv
source venv/bin/activate
pip install --upgrade pip wheel
pip install git+https://github.com/VulcanClimateModeling/fv3config.git

for config in ${CONFIG_LIST} ; do

    workdir=${rootdir}/rundir/bench_${compiler}_${config}
    if [ -d "${workdir}" ] ; then
        /bin/rm -rf "${workdir}"
    fi

    cd ${rootdir}/benchmarking/daint_single_node/
    ./run_benchmark.py \
        --hyperthreading \
        --threads_per_rank=4 \
        --nodes_per_tile_side=1 \
        --rank_layout=2 \
        --timesteps=${TIMESTEPS} \
        --force \
        --partition=${partition} \
        --executable=${FV3_EXE_DIR}/${compiler}/${FV3_EXE_NAME} \
        --module_env=${FV3_EXE_DIR}/${compiler}/module.env \
        --wait \
        config/${config}.yml ${workdir}

    set +e
    grep '^4-Termination ' ${workdir}/slurm-*.out > /dev/null
    if [ $? -ne 0 ] ; then
        exitError 715 ${LINENO} "Configuration ${config} did not run through (see `pwd`/${jobfile}.out)"
    fi
    set -e

    cp ${FV3_EXE_DIR}/${compiler}/git.env ${workdir}/

done

deactivate

# end timer and report time taken
T="$(($(date +%s)-T))"
printf "####### time taken: %02d:%02d:%02d:%02d\n" "$((T/86400))" "$((T/3600%24))" "$((T/60%60))" "$((T%60))"

# no errors encountered
echo "####### finished: $0 $* (PID=$$ HOST=$HOSTNAME TIME=`date '+%D %H:%M:%S'`)"
exit 0

# so long, Earthling!
