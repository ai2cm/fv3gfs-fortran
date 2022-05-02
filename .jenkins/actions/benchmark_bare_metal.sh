#!/bin/bash

# This script provides an Jenkins action to run a performance benchmark
# using pre-compiled executables. Several environment variables (see below)
# are expected to be set upon execution.
# If the benchmark runs through successfully, the timing results are converted
# to JSON format and stored. A *.tar.gz file of the latest successful run is
# also stored.

# 2021/01/22 Oliver Fuhrer, Vulcan Inc, oliverf@vulcan.com

# stop on all errors (also on errors in a pipe-redirection)
set -e
set -o pipefail

# the following environment variables need to be set
#   CONFIGURATION_LIST - Space-separated list of configurations to run benchmarks of
#   FV3_EXECUTABLE     - Name of executable to use (these executables are stored under /project/s1053/install/fv3gfs-fortran.
#   TIMESTEPS          - Number of timesteps to run benchmark for.

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

# make sure environment is sane
FV3_EXE_DIR=${installdir}/fv3gfs-fortran/
PERFORMANCE_DIR=${installdir}/../performance/fv3core_performance/fortran
if [ ! -d "${FV3_EXE_DIR}" ] ; then
    exitError 400 ${LINENO} "The directory FV3_EXE_DIR=${FV3_EXE_DIR} does not exist."
fi
if [ ! -d "${PERFORMANCE_DIR}" ] ; then
    exitError 410 ${LINENO} "The directory PERFORMANCE_DIR=${PERFORMANCE_DIR} does not exist."
fi
if [ -z "${CONFIGURATION_LIST}" ] ; then
    exitError 420 ${LINENO} "The variable CONFIGURATION_LIST=${CONFIGURATION_LIST} is not set."
fi
if [ -z "${FV3_EXECUTABLE}" ] ; then
    exitError 430 ${LINENO} "The variable FV3_EXECUTABLE=${FV3_EXECUTABLE} is not set."
fi
if [ -z "${TIMESTEPS}" ] ; then
    exitError 440 ${LINENO} "The variable TIMESTEPS=${TIMESTEPS} is not set."
fi

# run the benchmarks
# note: this relies on the fact that daint_gnu and daint_intel are the first
#       two options for the configure script
echo "### run benchmarks"

echo "==== module list ===="
module list
echo "====================="

# create venv for the two Python tools below
# note: can't use vcm_1.0 since we want to keep the module environment clean
module load cray-python/3.9.4.1
export FSSPEC_GS_REQUESTER_PAYS=vcm-ml
python3 -m venv venv
source venv/bin/activate
pip install --upgrade pip wheel
pip install -r requirements.txt
pip install click

cd ${rootdir}/benchmarking/daint_single_node/

for config in ${CONFIGURATION_LIST} ; do

    # create directory for benchmark
    work_name=bench_${compiler}_${config}
    work_dir=${rootdir}/rundir/${work_name}

    # create the run directory (will erase pre-existing directories) and run benchmark
    # note: this waits until completion
    partition=normal
    ./run_benchmark.py \
        --hyperthreading \
        --threads_per_rank=4 \
        --nodes_per_tile_side=1 \
        --rank_layout=2 \
        --timesteps=${TIMESTEPS} \
        --force \
        --partition=${partition} \
        --executable=${FV3_EXE_DIR}/${compiler}/${FV3_EXECUTABLE} \
        --module_env=${FV3_EXE_DIR}/${compiler}/module.env \
        --wait \
        config/${config}.yml ${work_dir} | tee /tmp/config_$$.log
    mv /tmp/config_$$.log ${work_dir}/config_$$.log

    # check for successful completion of run
    set +e
    grep '^4-Termination ' ${work_dir}/slurm-*.out > /dev/null
    if [ $? -ne 0 ] ; then
        exitError 715 ${LINENO} "Configuration ${config} did not run through (see `pwd`/${jobfile}.out)"
    fi
    set -e

    # copy meta-data
    if [ ! -f ${work_dir}/module.env ] ; then
        cp ${FV3_EXE_DIR}/${compiler}/module.env ${work_dir}/module.env
    fi
    cp ${FV3_EXE_DIR}/${compiler}/git.env ${work_dir}/git.env

    # convert to JSON file and store
    ./stdout_to_json.py ${work_dir} | tee /tmp/perf_$$.json
    mv /tmp/perf_$$.json ${PERFORMANCE_DIR}/`date +%Y-%m-%d-%H-%M-%S`_${compiler}_${config}.json

    # copy latest run to /project
    tarfile=${PERFORMANCE_DIR}/latest/${work_name}.tar.gz
    if [ -f ${tarfile} ] ; then
        /bin/rm -f ${tarfile}
    fi
    mkdir -p `dirname ${tarfile}`
    (cd ${work_dir}/..; tar cvfz ${tarfile} ${work_name})

done

cd -

deactivate

# end timer and report time taken
T="$(($(date +%s)-T))"
printf "####### time taken: %02d:%02d:%02d:%02d\n" "$((T/86400))" "$((T/3600%24))" "$((T/60%60))" "$((T%60))"

# no errors encountered
echo "####### finished: $0 $* (PID=$$ HOST=$HOSTNAME TIME=`date '+%D %H:%M:%S'`)"
exit 0

# so long, Earthling!
