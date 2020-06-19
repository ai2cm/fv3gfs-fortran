#!/bin/bash

# This script preproceses all Fortran files in the FV3 directory
# for serialization. It looks for the !$ser directive and only
# processes files which contain it. The original file is moved
# to *.orig

# Note: This script is setup to run inside a container with a
# serialbox2 installation under /serialbox2/

# Note 2: This script needs to be called with a non-empty string
# as an argument to actually do something

# Oliver Fuhrer, 6/19/20, Vulcan In.

if [ -z "$1" ] ; then
    echo "Exiting since $0 has been called without arguments"
    exit 0
fi

process_dir="./"
serialbox_dir=/serialbox2/
ppser_exe=${serialbox_dir}/python/pp_ser/pp_ser.py
ppser_flags="--verbose --ignore-identical -m utils_ppser_kbuff"

# check for pp_ser.py
if [ ! -d "${serialbox_dir}" ] ; then
    echo "ERROR: serialbox2 installation not found in ${serialbox_dir}"
    exit 1
fi
if [ ! -f "${ppser_exe}" ] ; then
    echo "ERROR: pp_ser.py installation not found in ${serialbox_dir}"
    exit 1
fi

# we need python3
if [ -z "`which python3`" ] ; then
    apt-get update && apt-get install -y python3
fi

# do the pre-processing
for d in `find ${process_dir} -type d`; do
    for f in `grep -sil '^ *!$ser' ${d}/*.[fF]{,90}` ; do
        echo "Preprocessing for serialization: ${f}"
	/bin/mv ${f} ${f}.orig
	python3 ${ppser_exe} ${ppser_flags} --output=${f} ${f}.orig
    done
done

exit 0
