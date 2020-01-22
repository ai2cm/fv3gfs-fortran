
REF_DIR=$1
CWD=$(pwd)

for dir in $CWD/output/*/
do
    echo $dir
    run_name=$(basename ${dir})
    mkdir -p $REF_DIR/$run_name
    cd $dir && md5sum *.nc RESTART/*.nc > $REF_DIR/$run_name/md5.txt
done
