#!/bin/bash

# This is a utility script which allows to generate serialized data for multiple
# experiment files matching a certain search pattern.

# Environment variables:
# EXPERIMENT_PATTERN  search pattern for experiment files (default: *.yml)
# FORTRAN_VERSION     override value in Makefile (see docu there)
# VALIDATE_ONLY       only compare data against version stored in cloud (instead of pushing)

# stop on all errors
set -e

# directory containing the experiment files
EXPERIMENT_DIR=configs

# use default pattern if no environment varaible is set
if [ -z "${EXPERIMENT_PATTERN}" ] ; then
    EXPERIMENT_PATTERN="*.yml"
fi

# unset FORTRAN_VERSION, if empty
if [ -z "${FORTRAN_VERSION}" ] ; then
    unset FORTRAN_VERSION
else
    set +e
    echo "${FORTRAN_VERSION}" | grep -E '^([a-zA-Z0-9]+-)?[0-9]+\.[0-9]+\.[0-9]+$'
    if [ $? -ne 0 ] ; then
        echo "Error: FORTRAN_VERSION does not adhere to the [<tag>-]X.Y.Z convention"
	echo "Note: <tag> can contain only alphanumeric characters and X, Y, Z have to be numbers"
        exit 1
    fi
    set -e
fi

# generate list of experiments (and abort if none found)
EXPERIMENTS=${EXPERIMENT_DIR}/${EXPERIMENT_PATTERN}
if [ -z "${EXPERIMENTS}" ] ; then
    echo "Warning: No matching experiment files for pattern ${EXPERIMENT_PATTERN} in ${EXPERIMENT_DIR} found."
    exit 1
fi

# loop over experiments
for exp_file in ${EXPERIMENTS} ; do
  exp_name=`basename ${exp_file} .yml`
  echo "====================================================="
  echo "Generating data for ${exp_name} ..."
  if [ "${VALIDATE_ONLY}" == "true" ] ; then
      EXPERIMENT=${exp_name} make generate_data validate_data
  else
      EXPERIMENT=${exp_name} make generate_data pack_data push_data
  fi
  echo "====================================================="
  echo ""
done

exit 0

