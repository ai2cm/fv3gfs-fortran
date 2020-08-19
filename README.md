[![CircleCI](https://circleci.com/gh/VulcanClimateModeling/fv3gfs.svg?style=svg)](https://circleci.com/gh/VulcanClimateModeling/fv3gfs)

# How to run fv3gfs via a docker image

### Step 1: Create Docker image

Different docker images are used to build the environment and compile this
model. For development purposes, the software environment is useful, but for
running the model it is most convenient that the docker image contains the
built FV3 model. Both of these images can be built using the following command:

#Creating the environment and compiled Docker images
```
make build
```
This will make a `fv3gfs-environment:<compile>_<mpi>_<cuda>` image with the current 
version of the environment using the specified compiler, MPI library and CUDA version.
Likewise, a `fv3gfs-compiled:<compile>_<mpi>_<cuda>` image with the default compile 
options is created.  
```
make build-serialize
```
This will create the environment and compiled Docker images with serialization enabled.
Users can use these Docker images to create new serialized data for testing and/or
code development.

Other tag names and compile options can be set. For example:
```
make build_debug
```
Rules are provided for certain compile options. Check the Makefile for a list.

#Specifying different compiler, MPI library and CUDA versions
Build arguments are defined in Makefile that correspond to the compiler used for
library and executable creation, library providing Message Passing Interface 
support and version of CUDA (if any) used.  The arguments are detailed below:

MPI - refers to the MPI library used. Valid values are 'openmpi' and 'mpich'.
      Default value is openmpi

COMPILER - refers to the name of the compiler suite used to generate all compiled
           objects, libraries and executables.  Valid value is 'gnu' only presently.

CUDA - refers to the version of CUDA used.  Currently, the only allowed value is '10.1'

#Specifying a different OS base version
All Docker images are created using Ubuntu linux as the base operating system.  Docker
images with CUDA support **must** be of version Ubuntu 18.04. Users may use a newer
Ubuntu version, like 19.10, but CUDA support will be not provided.  The default version
of the base OS is Ubuntu 18.04.  One can change this by setting the following build
argument:

BASE_OS_IMAGE - refers to the version of the base OS used.  Default value is 'nvidia/cuda:10.1-devel-ubuntu18.04'
                which provides CUDA support.  Valid values with no CUDA support
                include 'ubuntu:18.04' and 'ubuntu:19.10'.

### Step 2: Install fv3config
```
pip3 install -r requirements.txt
```
will download required dependencies for running tests and using fv3config. You can
also manually install the fv3config package.

In order to use `run_docker.sh` below, you will also need to set the fv3config
cache directory using
```
export FV3CONFIG_CACHE_DIR=<location>
```
This should be added to your bashrc.

While fv3config still uses "options" for data, you will also need to download the
data cache using
```
python3 -m fv3config.download_data
```
This should be done after the cache directory is set.

### Step 3: Create run directory
Create or download an fv3config yaml configuration. Edit the configuration as needed.
Examples of such configurations are included in the tests under `tests/pytest/config`.

Once you have a configuration file, you can write a run directory in python using:
```python3
import fv3config
import yaml
config = yaml.safe_load(open('fv3config.yml', 'r'))
fv3config.write_run_directory(config, 'rundir')
```
This example is included in `examples/create_rundir.py`.

You also must put a `submit_job.sh` script in the run directory. You can copy the one
included in this repository.
```
cp submit_job.sh rundir/
```

Optionally edit `rundir/input.nml` and `rundir/diag_table` to modify the namelist used
for your run and the set of diagnostics the model will output. Ideally this should be
done instead by editing the `fv3config.yml` we used earlier.


### Step 4: Run the model
```
bash run_docker.sh us.gcr.io/vcm-ml/fv3gfs-compiled:default <rundir> $FV3CONFIG_CACHE_DIR
```
where <rundir> is a full path to the run directory.

# Developing the model

The `fv3gfs-environment` docker image is useful for development purposes where the model code will be recompiled repeatedly in an interactive fashion. To start a bash shell in a docker container with the FV3 source tree mounted at `/FV3`, run the command:

    make enter

If necessary, this will build the image, but it will overwrite the compiled sources
with a bind mount to your host filesystem. You will need to compile the model with
the filesystem bind-mounted in this way. This can be done automatically with:

    make compile_dev

# Testing the model

Tests are included in the `tests` subdirectory. You can see which ones run in
continuous integration by inspecting `.circleci/config.yml`. Regression tests are
performed for a set of reference configurations included in `tests/pytest/config`.
Please read the README in `tests/pytest` for more information about these regression
tests and how to update the reference checksums.

# Interactive development of serialized model in Docker

To enter the docker container with the FV3 sources tree mounted to `/FV3/original`,
run:

    make enter_serialize

When in the docker container, you can reprocess the sources and recompile the model with:

    cd /FV3 && make -C original serialize_preprocess && make libs fv3.exe

If you need to be able to see the preprocessed `atmos_cubed_sphere` sources on your
host filesystem, you can do that with:

    MOUNTS_SERIALIZE="-v $(pwd)/FV3:/FV3/original -v <local dir>:/FV3/atmos_cubed_sphere" make enter_serialize

Where `<local_dir>` is replaced with the directory on your host filesystem where you
want to see the preprocessed sources.

This workflow does not support viewing preprocessed sources in the root `/FV3` directory
on the host filesystem, because `/FV3` has a bind-mounted subdirectory `original` and
it is not possible to contain a bind-mounted subdirectory in a separately bind-mounted
directory. You could see these sources by setting `SERIALBOX_OUTDIR` to a different
directory (it is `/FV3` by default) that you have bind-mounted in to the container.

# Docker buildkit

If you are using a version of docker that supports it, you can enable buildkit by
setting `DOCKER_BUILDKIT=1` as an environment variable. This can be useful when building
docker targets in this repo, because it will avoid building mulit-stage targets that
are not required for the final image.
