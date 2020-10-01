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

# generate list of configurations (and abort if none found)
CONFIGS=${CONFIG_DIR}/${CONFIG_PATTERN}
if [ -z "${CONFIGS}" ] ; then
    echo "Warning: No matching configuration files for pattern ${CONFIG_PATTERN} in ${CONFIG_DIR} found."
    exit 1
fi

# loop over configurations
for config in ${CONFIGS} ; do
  config=`basename $config .yml`
  echo "====================================================="
  echo "Generating data for $config ..."
  if [ "${VALIDATE_ONLY}" == "true" ] ; then
      CONFIGURATION=$config make generate_data validate_data
  else
      CONFIGURATION=$config make generate_data pack_data push_data
  fi
  echo "====================================================="
  echo ""
done

exit 0

