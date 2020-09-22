#!/bin/bash
set -e
set -x

# Set up the compute node environment
module load daint-gpu
module add /project/d107/install/modulefiles/
module load cray-python gcloud 
gcloud auth configure-docker


# Set up the working directing for the c12 test
cd examples
. /project/d107/install/venv/sn_1.0/bin/activate
#python write_rundir.py ../tests/pytest/config/default.yml "./c12_test"
python -c 'from fv3config import write_run_directory; write_run_directory "../tests/pytest/config/default.yml" "./c12_test"'
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


#export FV3_CONTAINER=fv3gfs-compiled:gnu9_mpich314_nocuda
#gsutil copy gs://vcm-jenkins/fv3gfs-compiled_gnu9_mpich314_nocuda.tar.gz .
#gunzip fv3gfs-compiled_gnu9_mpich314_nocuda.tar.gz
#sarus load ./fv3gfs-compiled_gnu9_mpich314_nocuda.tar ${FV3_CONTAINER}
#module unload sarus

#sbatch --wait job_jenkins_sarus
#md5sum -c ../../tests/pytest/reference/circleci/default/md5.txt
#rm *.nc 
#rm RESTART/*.nc

# Run a c12 regression test for the GNU-8 Docker image
#export FV3_CONTAINER=fv3gfs-compiled:gnu8_mpich314_cuda101
#gsutil copy gs://vcm-ml-public/jenkins-tmp/fv3gfs-compiled_gnu8_mpich314_cuda101.tar.gz .
#gunzip fv3gfs-compiled_gnu8_mpich314_cuda101.tar.gz
#module load sarus
#sarus load ./fv3gfs-compiled_gnu8_mpich314_cuda101.tar ${FV3_CONTAINER}
#module unload sarus

#sbatch --wait job_jenkins_sarus
#md5sum -c ./md5.txt
