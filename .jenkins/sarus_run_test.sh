#!/bin/bash
set -e
set -x

tags="hpc hpc-serialize"

# Set up the compute node environment
module load daint-gpu
module add /project/s1053/install/modulefiles/
module load cray-python gcloud 
gcloud auth configure-docker


# Set up the working directing for the c12 test
# Using the standard python virtual environment for Piz Daint
# defined in https://github.com/VulcanClimateModeling/daint_venv
cd examples
. /project/s1053/install/venv/sn_1.0/bin/activate
pip install -r ../requirements.txt
python -c 'import fv3config; import yaml; fid=open("../tests/pytest/config/default.yml", "r"); config = yaml.safe_load(fid); fv3config.write_run_directory(config, "./c12_test")'
deactivate

# Set the working directory for the upcoming SLURM jobs
cd c12_test
cp ../../.jenkins/job_jenkins_sarus .
cp ../../tests/pytest/reference/circleci/default/md5.txt .
export SCRATCH_DIR=${PWD}


# Run c12 regression test on each Docker image
declare -a tags=("hpc" "hpc-serialize")
for tag in ${tags}; do
    # Copy archived version of the Docker image from a Google Storage Bucket
    tar_file=fv3gfs-compiled-${tag}.tar
    gsutil copy gs://vcm-jenkins/${tar_file}.gz .
    gunzip ${tar_file}.gz

    # Load archive Docker image into the local Sarus container registry
    export FV3_CONTAINER=fv3gfs-compiled:${tag}
    module load sarus
    sarus load ./${tar_file} ${FV3_CONTAINER}
    module unload sarus

    # Launch SLURM job
    sbatch --wait job_jenkins_sarus

    # Verify results
    md5sum -c ./md5.txt 

    # Clean up working directory
    rm *.nc
    rm RESTART/*.nc

done

