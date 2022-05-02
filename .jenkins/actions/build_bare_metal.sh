#!/bin/bash

# This script provides a Jenkins action to build bare metal executables
# on the Piz Daint system at CSCS. Several environment variables (see below)
# are expected to be set upon execution.
# If the build runs through successfully, the resulting executables are stored.

# 2021/01/22 Oliver Fuhrer, Vulcan Inc, oliverf@vulcan.com

# stop on all errors (also on errors in a pipe-redirection)
set -e
set -o pipefail

# the following environment variables need to be set
#   CONFIGURATION_LIST - Space-separated list of configurations to test executables with
#                        (yml file must reside in /tests/serialized_test_data_generation/configs)
#   EXECUTABLE_SUFFIX  - Suffix to add to executable name when copied to /project
#   EXECUTABLE_NAMES   - Names of executable to use for tests (space separated)

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

# load module tools
. ${envloc}/env/moduleTools.sh

# load git tools
. ${envloc}/env/gitTools.sh

# load scheduler tools
. ${envloc}/env/schedulerTools.sh

# make sure environment is sane
FV3GFS_EXE_DIR=${installdir}/fv3gfs-fortran/
if [ ! -d "${FV3GFS_EXE_DIR}" ] ; then
    exitError 400 ${LINENO} "The directory FV3GFS_EXE_DIR=${FV3GFS_EXE_DIR} does not exist."
fi
if [ -z "${CONFIGURATION_LIST}" ] ; then
    exitError 410 ${LINENO} "The variable CONFIGURATION_LIST=${CONFIGURATION_LIST} is not set."
fi
if [ -z "${EXECUTABLE_NAMES}" ] ; then
    exitError 420 ${LINENO} "The variable EXECUTABLE_NAMES=${EXECUTABLE_NAMES} is not set."
fi

# compile the model
# note: this relies on the fact that daint_gnu and daint_intel are the first
#       two options for the configure script
echo "### compile model"

compiler_number=0
case $compiler in
    gnu) compiler_number=1 ;;
    intel) compiler_number=2 ;;
esac
cd ${rootdir}/FV3
echo "${compiler_number}" | ./configure

echo "==== module list ===="
module list
echo "====================="

./compile GT4PY_DEV=Y

num_exe=`/bin/ls -1d *.exe | wc -l`
if [ "$num_exe" -lt 1 ] ; then
   exitError 700 ${LINENO} "No valid executables have been found after compilation"
fi
cd -

# build venv for fv3config
module load cray-python/3.9.4.1
export FSSPEC_GS_REQUESTER_PAYS=vcm-ml
python3 -m venv ${rootdir}/venv
source venv/bin/activate
pip install -r ${rootdir}/requirements.txt
pip list
deactivate

# install and run example
# note: we setup the rundir using fv3config in a separate script in order to keep
#       the environment of this script clean (no modules loaded etc.)
for config in ${CONFIGURATION_LIST} ; do
  for exe_name in ${EXECUTABLE_NAMES} ; do
    echo "### run check (${config} with ${exe_name} compiled by ${compiler})"

    configdir=${rootdir}/tests/serialized_test_data_generation/configs
    rundir=${rootdir}/rundir/${config}
    mkdir -p ${rundir}

    source ${rootdir}/venv/bin/activate
    write_run_directory ${configdir}/${config}.yml ${rundir}
    deactivate

    cd ${rundir}

    cp ${rootdir}/FV3/*.exe ./
    cp ${rootdir}/FV3/conf/modules.fv3 ./module.env

    sed -i 's|^ *months *= *[0-9][0-9]* *$|months = 0|g' input.nml
    sed -i 's|^ *minutes *= *[0-9][0-9]* *$|minutes = 0|g' input.nml
    sed -i 's|^ *seconds *= *[0-9][0-9]* *$|seconds = 0|g' input.nml
    if [[ "${exe_name}" == *"debug"* ]] ; then
        sed -i 's|^ *days *= *[0-9][0-9]* *$|days = 0|g' input.nml
        sed -i 's|^ *hours *= *[0-9][0-9]* *$|hours = 1|g' input.nml
    else
        sed -i 's|^ *days *= *[0-9][0-9]* *$|days = 0|g' input.nml
        sed -i 's|^ *hours *= *[0-9][0-9]* *$|hours = 12|g' input.nml
    fi

    jobfile=job
    cp ${envloc}/env/submit.${host}.${scheduler}  ./${jobfile}
    sed -i 's|set -x||g' ${jobfile}
    sed -i 's|<OUTFILE>|'"${jobfile}.out"'|g' ${jobfile}
    sed -i 's|<G2G>|'"source ./module.env; module list"'|g' ${jobfile}
    sed -i 's|<CMD>|'"srun -n 6 ./${exe_name}"'|g' ${jobfile}
    sed -i 's|<NAME>|'"fv3_test"'|g' ${jobfile}
    sed -i 's|<NTASKS>|12|g' ${jobfile}
    sed -i 's|<NTASKSPERNODE>|'"12"'|g' ${jobfile}
    sed -i 's|<CPUSPERTASK>|1|g' ${jobfile}
    sed -i 's|--time=.*$|--time=01:00:00|g' ${jobfile}

    set +e
    launch_job ${jobfile} 3000
    if [ $? -ne 0 ] ; then
        exitError 710 ${LINENO} "Problem with SLURM job (`pwd`/${jobfile}) see log (`pwd`/${jobfile}.out)"
    fi
    grep '^4-Termination ' job.out > /dev/null
    if [ $? -ne 0 ] ; then
        exitError 715 ${LINENO} "Configuration ${config} did not run through (see `pwd`/${jobfile}.out)"
    fi
    set -e

    cd -
    /bin/rm -rf "${rundir}"

  done
done

# copy executables to install dir (and add meta-information)
echo "### saving executables"
mkdir -p ${FV3GFS_EXE_DIR}/${compiler}
for f in ${rootdir}/FV3/*.exe ; do
    exe_name=`basename ${f} | sed 's/\.exe$//g'`
    cp ${f} ${FV3GFS_EXE_DIR}/${compiler}/${exe_name}${EXECUTABLE_SUFFIX}.exe
done
cp ${rootdir}/FV3/conf/modules.fv3 ${FV3GFS_EXE_DIR}/${compiler}/module.env
cat > ${FV3GFS_EXE_DIR}/${compiler}/git.env <<EOF2
GIT_URL = ${GIT_URL}
GIT_COMMIT = ${GIT_COMMIT}
GIT_BRANCH = ${GIT_BRANCH}
GIT_LOCAL_BRANCH = ${GIT_LOCAL_BRANCH}
EOF2

# end timer and report time taken
T="$(($(date +%s)-T))"
printf "####### time taken: %02d:%02d:%02d:%02d\n" "$((T/86400))" "$((T/3600%24))" "$((T/60%60))" "$((T%60))"

# no errors encountered
echo "####### finished: $0 $* (PID=$$ HOST=$HOSTNAME TIME=`date '+%D %H:%M:%S'`)"
exit 0

# so long, Earthling!
