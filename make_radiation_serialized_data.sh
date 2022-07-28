#!/bin/bash
./phys_build.sh

python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt

gsutil cp gs://vcm-ml-experiments/cloud-ml/2022-06-24/cloud-ml-training-data/fv3gfs_run/fv3config.yml .
sed -i '/override_surface_radiative_fluxes/d' fv3config.yml
sed -i 's/npx: 49/npx: 13/g' fv3config.yml
sed -i 's/npy: 49/npy: 13/g' fv3config.yml
sed -i 's/npz: 79/npz: 63/g' fv3config.yml
sed -i 's/days: 5/days: 0/g' fv3config.yml
sed -i 's/seconds: 0/seconds: 900/g' fv3config.yml
write_run_directory fv3config.yml rundir
cp example/submit_job.sh rundir/
cp example/clean.sh rundir/
export SER_ENV=RADIATION
./phys_run.sh