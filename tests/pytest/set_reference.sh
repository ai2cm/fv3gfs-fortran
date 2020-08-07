set -xe

IMG_TAG=$1
REF_DIR=$2
CWD=$(pwd)

for dir in $CWD/output/$IMG_TAG/*/
do
    echo $dir
    run_name=$(basename ${dir})
    mkdir -p $REF_DIR/$run_name
    echo $REF_DIR/$run_name
    cd $dir 
    md5sum *.nc RESTART/*.nc > $REF_DIR/$run_name/md5.txt
    if ls test_data/Gen*.dat test_data/*.json >/dev/null 2>&1; then
        md5sum test_data/Gen*.dat test_data/*.json > $REF_DIR/$run_name/md5_serialize.txt
    fi
done
