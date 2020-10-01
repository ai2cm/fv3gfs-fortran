#!/bin/bash

# This is a utility script which allows to generate serialized data for multiple
# configuration files matching a certain search pattern.

# Environment variables:
# CONFIG_PATTERN  search pattern for config files (default: *.yml)

# stop on all errors
set -e

# directory containing the configuration files
CONFIG_DIR=configs

# use default pattern if no environment varaible is set
if [ -z "${CONFIG_PATTERN}" ] ; then
    CONFIG_PATTERN="*.yml"
fi

# unset FORTRAN_VERSION, if empty
if [ -z "${FORTRAN_VERSION}" ] ; then
    unset FORTRAN_VERSION
fi

# generate list of configurations (and abort if none found)
CONFIGS=${CONFIG_DIR}/${CONFIG_PATTERN}
if [ -z "${CONFIGS}" ] ; then
    echo "Warning: No matching configuration files for pattern ${CONFIG_PATTERN} in ${CONFIG_DIR} found."
    exit 0
fi

# loop over configurations
for config in ${CONFIGS} ; do
  config_file=`basename $config .yml`
  echo "====================================================="
  echo "Generating data for $config_file ..."
  CONFIGURATION=$config_file make generate_and_push_data
  echo "====================================================="
  echo ""
done

