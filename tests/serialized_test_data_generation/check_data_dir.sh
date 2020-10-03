#!/bin/bash

# This script does a series of basic consistency checks on a directory
# containing serialized data. The intent is to make sure data is ok
# before using it for unit-testing or pushing it onto the cloud storage bucket.

if [ $# -ne 1 -o -z "$1" -o ! -d "$1" ] ; then
  echo "Error: must supply a directory with serialized data from a model run"
  exit 1
fi
dir=$1

echo ">> Checking consistency of data directory ${dir}"

# check for a basic set of files
files="fortran_sha.txt logfile.000000.out stdout.out stderr.out md5sums.txt input.nml env.out ArchiveMetaData-Generator_rank0.json MetaData-Generator_rank0.json"
for f in ${files} ; do
    if [ ! -f ${dir}/${f} ] ; then
        echo "Error: file ${dir}/${f} not present"
        exit 1
    fi
done

# make sure model run was successful
if [ `grep "Termination" ${dir}/logfile.000000.out | wc -l` -ne 1 ] ; then
    echo "Error: model run does not seem to have terminated successfully"
    exit 1
fi
if [ `grep "Termination" ${dir}/stdout.out | wc -l` -ne 1 ] ; then
    echo "Error: model run does not seem to have terminated successfully"
    exit 1
fi

# make sure serialization was switched on
if [ `grep "WARNING: SERIALIZATION IS ON" ${dir}/stdout.out | wc -l` -lt 1 ] ; then
    echo "Error: model run without serialization activated"
    exit 1
fi

# make sure GT4PY_DEV was switched on
if ! grep "WARNING: RUNNING WITH GT4PY_DEV ON" ${dir}/stdout.out &> /dev/null ; then
    echo "Error: model run without GT4PY_DEV=Y activated"
    exit 1
fi

# make sure reasonable number of *.json files exists and they are all > 0 bytes
if [ `/bin/ls ${dir}/*.json | wc -l` -lt 2 ] ; then
    echo "Error: less than 2 *.json files seems fishy"
    exit 1
fi
if [ `wc --bytes ${dir}/*.json | sort -n | head -1 | awk '{print $1}'` -eq 0 ] ; then
    echo "Error: found some *.json files which 0 Bytes in size"
    exit 1
fi

# make sure that all *.json files exists from all ranks
# explanation: <name>_rank0.json | extract  <name> | sort names | count occurrences of different <name>'s 
#                   | extract counts | sort counts | count occurrences of different counts | make sure there is only exactly 1
if [ `/bin/ls ${dir}/*.json | sed 's/_.*//g' | sort | uniq -c | awk '{print $1}' | sort | uniq -c | wc -l` -ne 1 ] ; then
  echo "Error: there seem to be an inconsistent number of *.json files"
  exit 1
fi

# make sure reasonable number of *.dat files exists and they are all > 0 bytes
if [ `/bin/ls ${dir}/*.dat | wc -l` -lt 10 ] ; then
    echo "Error: less than 10 *.dat files seems fishy"
    exit 1
fi
if [ `wc --bytes ${dir}/*.dat | sort -n | head -1 | awk '{print $1}'` -eq 0 ] ; then
    echo "Error: found some *.dat files which 0 Bytes in size"
    exit 1
fi

# make sure that all fields are serialized from all ranks
# explanation: Generator_rank<XXX>_<field>.dat | extract <field>.dat | sort fields | count occurrences of differnt <fields>'s
#                   | extract counts | sort counts | count occurrences of different counts | make sure thre is only exactly 1
if [ `/bin/ls ${dir}/*.dat | sed 's/.*rank[0-9]*_//g' | sort | uniq -c | awk '{print $1}' | sort -n | uniq -c | wc -l` -ne 1 ] ; then
    echo "Error: there seem to be different number of *.dat files for different data fields"
    exit 1
fi

# make sure that the data is at least 1024 KB
if [ `du -ks ${dir} | awk '{print $1}'` -lt 1024 ] ; then
    echo "Error: total size of data directory is less than 1024 KB"
    exit 1
fi

echo ">> Success"
exit 0
