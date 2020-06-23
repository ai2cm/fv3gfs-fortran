#!/bin/bash

# This script preproceses all Fortran files in the FV3 directory
# for serialization. It looks for the !$ser directive and only
# processes files which contain it. The original file is moved
# to *.orig

# Note: This script is setup to run inside a container with a
# serialbox2 installation under /serialbox2/, as part of the Dockerfile build process.
# Do not run this script manually unless you know what you are doing!

# Note 2: This script needs to be called with an input and output directory

# Oliver Fuhrer, 6/19/20, Vulcan Technologies LLC

if [ -z "$1" ] ; then
    echo "Exiting since $0 has been called without arguments"
    exit 0
fi

if [ -z "$2" ] ; then
    echo "Exiting since $0 has been called without twoarguments"
    exit 0
fi

process_dir=$1
output_dir=$2

if [ ! -f "${PPSER_PY}" ] ; then
    echo "ERROR: pp_ser.py installation not found at ${PPSER_PY} or PPSER_PY is unset"
    exit 1
fi

# we need python3
if [ -z "`which python3`" ] ; then
    echo "ERROR: python3 not found"
    exit 1
fi

for d in `find ${process_dir} -type d`; do
    for f in `grep -sil '^ *!$ser' ${d}/*.[fF]{,90}` ; do
        echo "Preprocessing for serialization: ${f} to ${output_dir}/${f#${process_dir}}"
        mkdir -p $(dirname ${f})
        python3 ${PPSER_PY} ${PPSER_FLAGS} --output=${output_dir}/${f#${process_dir}} ${f}
    done
done

exit 0