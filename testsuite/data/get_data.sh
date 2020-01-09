#!/bin/bash

# this scripts downloads test data COSMO technical testsuite

# command to get {FILE}
curl_path='curl -X GET -o {FILE} https://www.googleapis.com/storage/v1/b/vcm-ml-public/o/regression_test_data%2Ftestsuite%2Ffv3gfs%2F{FILE}?alt=media'

# list of all test type directories
test_type_list=('c48_basic')

# Input data file names on ftp server, some data files differs from test name. 
# The test_data_list[i] must be of the form test_type_list[i]+"optional identifier"
# The name of the test data file on ther server  must be input_$test_data_list[i].tar.gz
# ex: input_cosmoe_r2.tar.gz, for revision 2 of cosmoe input data
test_data_list=('c48_basic')

if [ "${#test_type_list[@]}" -ne "${#test_data_list[@]}" ]; then
    echo "Error test_type_list and test_data_names length differ"
    exit 1
fi

# go to data directory in case script is invoked from top-level directory
test -f src/testsuite.py
if [ $? -eq 0 ] ; then
  cd data
fi

echo "Getting data in `pwd`"
#loop over test type list
for i in "${!test_type_list[@]}"; do
    test_type=${test_type_list[i]}
    test_data=${test_data_list[i]}
    # check test_type and test_data are compatible names
    if [[ $test_data != *"$test_type"* ]]; then
	echo "Error incompatible type and data names: $test_type $test_data"
	exit 1
    fi
    test -d $test_type || exit 1
    set -x
    cd $test_type
    # remove input if exists
    /bin/rm -rf input*
    filename=input_${test_data}.tar.gz
    cmd=`echo ${curl_path} | sed 's/{FILE}/'${filename}'/g'`
    ${cmd}
    test -f ${filename} || exit 1
    tar -xf ${filename} || exit 1
    /bin/rm -f ${filename} 2>/dev/null
    cd ..
    set +x
done

echo "Data transfer completed"
# done
