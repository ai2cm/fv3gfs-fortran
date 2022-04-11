#!/bin/bash

# Script which generates coverage information given a configuration *.yml file.
# 
# Usage: coverage.sh default.yml
# Output: coverage_default directory which contains coverage in html format
#
# Author: Oliver Fuhrer, Vulcan Inc.

# stop on errors
set -e

# arguments
config_file=$1
if [ -z "${config_file}" -o ! -f "${config_file}" ] ; then
  echo "Error: must specify a valid config file as argument (usage: $0 config.yml)"
  exit 1
fi

# setup
base_dir=`pwd`
config_name=`basename ${config_file} .yml`
coverage_dir=${base_dir}/coverage_${config_name}
rundir=${base_dir}/rundir  # must be an absolute path
submit_job=${base_dir}/tests/serialized_test_data_generation/submit_job.sh
count_ranks=${base_dir}/tests/serialized_test_data_generation/count_ranks.py
gcr_url="us.gcr.io/vcm-ml/fv3gfs-compiled"

# don't overwrite any previous coverage data
if [ -d ${coverage_dir} ] ; then
    echo "The directory ${coverage_dir} already exists. Remove and try again."
    exit 1
fi

# build docker container with model compiled for coverage
COMPILED_TAG_NAME=gcov COMPILED_IMAGE=${gcr_url}:gcov COMPILE_OPTION="OPENMP=\\\nREPRO=\\\nDEBUG=Y\\\nGCOV=Y\\\nGT4PY_DEV=Y\\\nSUBSET_PHYSICS=Y" make -C "${base_dir}" build

# setup run directory
\rm -rf "${rundir}"
write_run_directory "${config_file}" "${rundir}"
cp "${submit_job}" "${rundir}"
cp "${count_ranks}" "${rundir}"

# run the model
mkdir -p ${coverage_dir}; \rm -rf ${coverage_dir}/*
mkdir -p ./data; \rm -rf ./data/*
docker run -it --rm --network host -v ${coverage_dir}:/coverage -v `pwd`/rundir:/rundir -v `pwd`/data:/data ${gcr_url}:gcov bash -c "set -ex; cd /rundir; ./submit_job.sh; pip3 install gcovr; cd /coverage; gcovr -d -r /FV3/ --html --html-details -o index.html"
\rm -rf data

# cleanup run directory
set +e
grep Termination ${rundir}/stdout.out > /dev/null
if [ $? -ne 0 ] ; then
  echo "Warning: Run does not seem to have been successfull. Check the rundir!"
else
  \rm -rf "${rundir}"
fi
set -e

# tell user we're done
echo "Open coverage_${config_name}/index.html to view coverage data"
exit 0

