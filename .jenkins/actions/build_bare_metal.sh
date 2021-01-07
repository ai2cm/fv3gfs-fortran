#!/bin/bash

set -x
set -e

INSTALL_DIR=${PROJECT}/../install
FV3GFSEXE_DIR=${INSTALL_DIR}/fv3gfs-fortran/
FV3CONFIG_VENV=${INSTALL_DIR}/venv/vcm_1.0/

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
set +e
. ${envloc}/env/machineEnvironment.sh
set -e

# load module tools
. ${envloc}/env/moduleTools.sh

# load git tools
. ${envloc}/env/gitTools.sh

# load scheduler tools
. ${envloc}/env/schedulerTools.sh

# compile the model
echo "### compile model"
compiler_number=0
case $compiler in
  gnu) compiler_number=1 ;;
  intel) compiler_number=2 ;;
esac
cd ${rootdir}/FV3
echo "${compiler_number}" | ./configure
./compile
cd -

# install and run example
echo "### run install and example"
script=/tmp/create_rundir_$$.sh
for config in c12_6ranks_standard c48_6ranks_standard ; do
    cat > ${script} <<EOF
#!/bin/bash
set -e
export GOOGLE_APPLICATION_CREDENTIALS=/users/olifu/.gc_cred/jenkins-sa-key.json
cd ${rootdir}/tests/serialized_test_data_generation/
source ${FV3CONFIG_VENV}/bin/activate
module load gcloud
EXPERIMENT=${config} make setup_rundir
deactivate
cd -
EOF
    chmod 755 ${script}
    ${script}
    cd ${rootdir}/tests/serialized_test_data_generation/rundir/${config}/
    cp ${rootdir}/FV3/*.exe ./
    script=job
    cp ${envloc}/env/submit.${host}.${scheduler}  ./${script}
    sed -i 's|<OUTFILE>|'"job.out"'|g' ${script}
    sed -i 's|<G2G>|'""'|g' ${script}
    sed -i 's|<CMD>|'"srun -n 6 ./fv3_64bit.exe"'|g' ${script}
    sed -i 's|<NAME>|'"fv3_test"'|g' ${script}
    sed -i 's|<NTASKS>|12|g' ${script}
    sed -i 's|<NTASKSPERNODE>|'"12"'|g' ${script}
    sed -i 's|<CPUSPERTASK>|1|g' ${script}
    launch_job ${script} 3000
done

# copy executables to install dir
echo "### saving executables"
mkdir -p ${INSTALL_DIR}/fv3gfs-fortran/${compiler}
cp ${rootdir}/FV3/*.exe ${INSTALL_DIR}/fv3gfs-fortran/${compiler}/

# end timer and report time taken
T="$(($(date +%s)-T))"
printf "####### time taken: %02d:%02d:%02d:%02d\n" "$((T/86400))" "$((T/3600%24))" "$((T/60%60))" "$((T%60))"

# no errors encountered
echo "####### finished: $0 $* (PID=$$ HOST=$HOSTNAME TIME=`date '+%D %H:%M:%S'`)"
exit 0

# so long, Earthling!
