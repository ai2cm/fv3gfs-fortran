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
make build_debug
```
Rules are provided for certain compile options. Check the Makefile for a list.

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
