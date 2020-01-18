[![CircleCI](https://circleci.com/gh/VulcanClimateModeling/fv3gfs.svg?style=svg)](https://circleci.com/gh/VulcanClimateModeling/fv3gfs)

# How to run fv3gfs via a docker image

### Step 1: Create Docker image

Different docker images are used to build the environment and compile this
model. For development purposes, the software environment is useful, but for
running the model it is most convenient that the docker image contains the
built FV3 model. Both of these images can be built using the following command:

```
make build
```
This will make a `fv3gfs-environment:latest` image with the current version of
the environment and a `fv3gfs-compiled:default` image with the default compile options.
Other tag names and compile options can be set. For example:
```
COMPILED_TAG_NAME=32bit COMPILE_OPTION=32BIT=Y make build
```
Rules are provided for certain compile options. For example `make build_32bit` is equivalent
to the above.

### Step 2: Download input data
```
bash download_inputdata.sh
```
will download required input data and files using wget from the vcm-ml-public Google storage bucket.

### Step 3: Create experiment case and configure
```
bash create_case.sh <experiment-name>
```
where `<experiment-name>` is whatever you would like to call your experiment.

Optionally edit `experiments/<experiment-name>/rundir/input.nml` and `experiments/<experiment-name>/rundir/diag_table` to modify the namelist used for your run and the set of diagnostics the model will output.

### Step 4: Run the model
```
bash run_docker.sh fv3gfs-compiled:default <experiment-name>
```
Output data specified in `diag_table` and log files (including stdout and stderr) will be saved in `experiments/<experiment-name>/rundir`. By default the container will run in the background.

## Google Container Repository

By default Circle CI builds two configurations and pushes them to Google
Container Repository:

- `us.gcr.io/vcm-ml/fv3gfs-compiled:32bit`. A 32bit version of the model.
- `us.gcr.io/vcm-ml/fv3gfs-compiled:default`. A default version compiled with
  64 bit reals.

# Developing the model

The `fv3gfs-environment` docker image is useful for development purposes where the model code will be recompiled repeatedly in an interactive fashion. To start a bash shell in a docker container with the FV3 source tree mounted at `/FV3`, run the command:

    make enter
If necessary, this will build the image, but it will not compile the source code.

# fv3gfs
The public home of NOAA EMC's FV3GFS Modeling System.

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or
favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

The code is supported on NOAA research and development machines THEIA, GAEA and Jet, where prebuilt libraries and test cases are available. Best efforts will be provided to support users to run this public release code on those machines.
