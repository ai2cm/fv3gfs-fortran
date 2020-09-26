[![VulcanClimateModeling](https://circleci.com/gh/VulcanClimateModeling/fv3gfs-fortran.svg?style=svg)](https://circleci.com/gh/VulcanClimateModeling/fv3gfs-fortran)

# How to run fv3gfs via a docker image

## Step 1: Create Docker image

Different docker images are used to build the environment and compile this
model. For development purposes, the software environment is useful, but for
running the model it is most convenient that the docker image contains the
built FV3 model. Both of these images can be built using the following command:

```bash
make build
```

This will make a `fv3gfs-compiled:latest` image with a compiled model executable
that can be used for production simuliations. Other tag names and compile options can be set. For example, to build a debugging executable in a docker image:

```bash
make build_debug
```

Rules are provided for certain compile options. Check the Makefile for a list or simply type `make help`.

## Step 2: Install fv3config

```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
```

will download required dependencies for running tests and using fv3config. You can
also manually install the fv3config package.

In order to use `run_docker.sh` below, you will also need to set the fv3config
cache directory using

```bash
export FV3CONFIG_CACHE_DIR=<location>
```

This should be added to your `~/.bashrc`. If you do not set your cache directory, a default location will be used. You can use the command

```bash
python -c 'import fv3config; print(fv3config.get_cache_dir())'
```

to find out the default location.

## Step 3: Create run directory

Create or download an fv3config yaml configuration. Edit the configuration as needed.
Examples of such configurations are included in the tests under `tests/pytest/config`.

Once you have a configuration file, e.g. `example/config.yml`, you can write a run directory in python using:

```python3
import fv3config
import yaml
config = yaml.safe_load(open('example/config.yml', 'r'))
fv3config.write_run_directory(config, 'rundir')
```

This example is included in `example/create_rundir.py`.

You also must put a `submit_job.sh` script in the run directory. You can copy the one
included in this repository.

```bash
cp example/submit_job.sh rundir/
```

Optionally edit `rundir/input.nml` and `rundir/diag_table` to modify the namelist used
for your run and the set of diagnostics the model will output. Ideally this should be
done instead by editing the `config.yml` we used earlier.


## Step 4: Run the model

```bash
bash run_docker.sh us.gcr.io/vcm-ml/fv3gfs-compiled:latest <rundir> $FV3CONFIG_CACHE_DIR
```

where `<rundir>` is an absolute path to the run directory.

# Developing the model

The `fv3gfs-environment` docker image is useful for development purposes where the model code will be recompiled repeatedly in an interactive fashion. To start a bash shell in a docker container with the FV3 source tree mounted at `/FV3`, run the command:

```bash
make enter
```

If necessary, this will build the image, but it will overwrite the compiled sources
with a bind mount to your host filesystem. You will need to compile the model with
the filesystem bind-mounted in this way. Once in the container, you can compile the model using the commands

To compile the model and generate an executable you can use the commands

```bash
cd /FV3
make clean
make -j8
```

# Testing the model

Tests are included in the `tests` subdirectory. You can see which ones run in
continuous integration by inspecting `.circleci/config.yml`.

Regression tests which check the bit-reproducibility of results are
performed for a set of reference configurations included in `tests/pytest/config`.
Please read the README in `tests/pytest` for more information about these regression
tests and how to update the reference checksums.

# Serialization

The model can be compiled for the generation of serialize data which can be used for unit-testing individual components of the model. For more information see the documentation [here](tests/serialized_test_data_generation/README.md).

To build a docker image for serialization use the command

```bash
make build_serialize
```

which will create an image where the source files have been pre-processed for serialization. Similarly to default compilation, you can interactively develop
for serialization using the command

```bash
make enter_serialize
```

The original FV3 sources will be mounted to `/FV3/original`. To compile the model for serialization inside the container, you can use the commands

```bash
cd /FV3
make clean
make -C original serialize_preprocess
make -j8
```

If you need to be able to see the preprocessed `atmos_cubed_sphere` sources on your
host filesystem, you can do that with:

```bash
MOUNTS_SERIALIZE="-v $(pwd)/FV3:/FV3/original -v <local dir>:/FV3/atmos_cubed_sphere" make enter_serialize
```

Where `<local_dir>` is replaced with the directory on your host filesystem where you
want to see the preprocessed sources.

This workflow does not support viewing preprocessed sources in the root `/FV3` directory
on the host filesystem, because `/FV3` has a bind-mounted subdirectory `original` and
it is not possible to contain a bind-mounted subdirectory in a separately bind-mounted
directory. You could see these sources by setting `SERIALBOX_OUTDIR` to a different
directory (it is `/FV3` by default) that you have bind-mounted in to the container.

# Docker BuildKit

If you are using a version of docker that supports it, you can enable buildkit by
setting `DOCKER_BUILDKIT=1` as an environment variable. This can be useful when building docker targets in this repo, because it will avoid building mulit-stage targets that are not required for the final image.
