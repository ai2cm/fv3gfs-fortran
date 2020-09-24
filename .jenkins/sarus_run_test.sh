#!/bin/bash
set -e
set -x

# Set up the compute node environment
module load daint-gpu
module add /project/d107/install/modulefiles/
module load cray-python gcloud 
gcloud auth configure-docker


# Set up the working directing for the c12 test
# Using the standard python virtual environment for Piz Daint
# defined in https://github.com/VulcanClimateModeling/daint_venv
cd examples
. /project/d107/install/venv/sn_1.0/bin/activate
pip install -r ../requirements.txt
python -c 'import fv3config; import yaml; fid=open("../tests/pytest/config/default.yml", "r"); config = yaml.safe_load(fid); fv3config.write_run_directory(config, "./c12_test")'
deactivate

cd ${PWD}/c12_test
cp ../../.jenkins/job_jenkins_sarus .
cp ../../.jenkins/md5*.txt .
export SCRATCH_DIR=${PWD}


# Run a c12 regression test for each Docker image
dockerfiles=( $( ls ../../docker/Dockerfile.* ) )
for df in ${dockerfiles[@]}; do
   # Grab the architecture tag from the dockerfile filename
   arch=${df:24}

   # Get the name of the Docker image and tar file
   export FV3_CONTAINER=fv3gfs-compiled:${arch}
   tar_file=fv3gfs-compiled_${arch}.tar

   # Copy archived version of the Docker image from a Google Storage Bucket
   gsutil copy gs://vcm-jenkins/${tar_file}.gz .
   gunzip ${tar_file}.gz

   # Load archive Docker image into the local Sarus container registry
   module load sarus
   sarus load ./${tar_file} ${FV3_CONTAINER}
   module unload sarus

   # Launch SLURM job
   sbatch --wait job_jenkins_sarus

   # Verify results
   md5sum -c ./md5_${arch}.txt

   # Clean up working directory
   rm *.nc
   rm RESTART/*.nc
done

