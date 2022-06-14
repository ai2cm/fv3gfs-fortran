[![CircleCI](https://circleci.com/gh/ai2cm/fv3gfs-fortran/tree/master.svg?style=svg)](https://circleci.com/gh/ai2cm/fv3gfs-fortran/tree/master)

# Repository Structure

This repository structure corresponds loosely to the top-level structure of
the [UFS](https://github.com/ufs-community/ufs-weather-model), but with many
submodules replaced by subtrees for convenience. See this
[description](https://github.com/ufs-community/ufs-weather-model) of the
submodule hierarchy of the UFS. This table shows the canonical upstream
repositories associated with different subtrees of this repository:

```
.                          # ~ https://github.com/ufs-community/ufs-weather-model
├── FMS                    # https://github.com/NOAA-GFDL/FMS
├── FV3                    # https://github.com/NOAA-EMC/fv3atm
│   ├── atmos_cubed_sphere # https://github.com/NOAA-GFDL/GFDL_atmos_cubed_sphere
│   └── ccpp
│       ├── framework      # https://github.com/NCAR/ccpp-framework
│       └── physics        # https://github.com/NCAR/ccpp-physics
├── serialbox              # https://github.com/GridTools/serialbox
└── stochastic_physics     # https://github.com/noaa-psd/stochastic_physics
```

In some cases these are actual submodules, and in other cases they are
subtrees.

# How to run fv3gfs via a docker image

Don't forget to load the submodules in your local copy of the source, i.e.

```bash
cd fv3gfs-fortran
git submodule update --init --recursive
```

## Step 1: Create Docker image

Different docker images are used to build the environment and compile this
model. For development purposes, the software environment is useful, but for
running the model it is most convenient that the docker image contains the
built FV3 model. Both of these images can be built using the following command:

```bash
make build
```

This will make a `fv3gfs-compiled:gnu7-mpich314-nocuda` image with a compiled model executable
that can be used for production simuliations. Other tag names and compile options can be set. For example, to build a debugging executable in a docker image:

```bash
make build_debug
```

Rules are provided for certain compile options. Check the Makefile for a list or simply type `make help`.

## Step 2: Install fv3config

FV3 expects to be run within a certain directory structure, which we call a "run directory". These can be prepared using the [fv3config](https://github.com/ai2cm/fv3config).

This tool can be installed using `pip install fv3config`. To use versions that are guaranteed to work, we provide a `requirements.txt`, that you can use as follows:

```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
```

This command will download required dependencies for running tests and using fv3config.

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

Once you have a configuration file, e.g. `example/config.yml`, you can write a run directory as follows:

    write_run_directory example/config.yml rundirectory

You will need to be authenticated with google cloud storage to run this command. See instructions below.

The python API equivalent of this is
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


### Authentication

The data referred to by this example configuration are stored in a GCS bucket, stored in the `us-central1` region. This data is free to use, but we have enabled requestor-pays to avoid paying for network transfer costs incurred by external users. You will need to authenticate with your own google cloud project credentials to access this data. Detailed instructions are out of scope, but usually involves the setting the following environmental variables
```
export FSSPEC_GS_REQUESTER_PAYS="on"
export GOOGLE_APPLICATION_CREDENTIALS=/path/to/key.json
```
For more information see [this documentation](https://gcsfs.readthedocs.io/en/latest/api.html#gcsfs.core.GCSFileSystem).

## Step 4: Run the model

## Within container/manual

Assuming you are in environment with a compiled version of the FV3 model at the path `/absolute/path/to/fv3.exe` and a rundirectory at the path `<rundir>`, you can run the model like this

    cd <rundir>
    mpirun -n <number of processors> /absolute/path/to/fv3.exe

The number of processors has to be `6 * num_tiles` where `num_tiles` is the product of the `namelist.fv_core_nml.layout` configurations.


## From host via docker

If you would like to run the model in one of the included docker containers use the command

```bash
bash run_docker.sh <image> <rundir> $FV3CONFIG_CACHE_DIR
```

where `<rundir>` is an absolute path to the run directory. `<image>` is the name of the container built by `make build`, typically `us.gcr.io/vcm-ml/fv3gfs-compiled:gnu7-mpich314-nocuda`.

# Developing the model

## Docker

The `fv3gfs-environment` docker image is useful for development purposes where the model code will be recompiled repeatedly in an interactive fashion. To start a bash shell in a docker container with the FV3 source tree mounted at `/FV3`, run the command:

```bash
make enter
```

If necessary, this will build the image, but it will overwrite the compiled sources
with a bind mount to your host filesystem. You will need to compile the model with
the filesystem bind-mounted in this way. Once in the container, you can compile the model using the commands
```bash
cd /FV3
./configure gnu_docker
make clean
make -j8
```

## Nix



FV3 can also be installed using the [nix](https://nixos.org/) package
manager. This package manager is available on Mac and Linux, and provides a
light weight means to distribute isolated software environments. 


### Installation

To begin, install nix following [these instructions](https://nixos.org/download.html).

(optional) We host binaries using a tool called cachix, and this will greatly speed up any builds. To use our binaries, [install cachix](https://github.com/cachix/cachix#installation) and then run

    cachix use vulcanclimatemodeling

Without using the cachix cache, FV3 and all its dependencies will need to build from source (~20 minutes). This only happens once per machine, but it is slow.

### Developing

To develop the model, you can use the environment specified in `shell.nix` by running

    nix-shell

Then configure the build to use nix

    cd FV3 && ./configure nix

And build the model (with coverage outputs, from the root directory)

    make build_native
    
And the wrapper

    make -C FV3 wrapper_build

At this point you can run [the native tests](#native-tests).
# Testing the model

Tests are included in the `tests` subdirectory. You can see which ones run in
continuous integration by inspecting `.circleci/config.yml`.

Regression tests which check the bit-reproducibility of results are
performed for a set of reference configurations included in `tests/pytest/config`.
Please read the README in `tests/pytest` for more information about these regression
tests and how to update the reference checksums.

## Native tests

Some of the tests can be run in a native environment (e.g. nix, bare metal,
or inside a docker container). A native environment is capable of running the
file `FV3/fv3.exe`.

After building the model inside of the `FV3` subdirectory, you can run these
tests like this:

    make test_native
    # or manually
    pytest --native tests/pytest
    # and 
    pytest FV3/wrapper/tests/

When using the makefile target, code coverages reports will be saved to the
folder `coverage_<timestamp>`.

## Image tests

Certain tests are more convenient or reliable to run within a docker container.
For example, regression tests are only valid within a consistent environment as
represented by a docker container. These test are run from a host environment like this

    pytest tests/pytest

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

Make sure to set the GT4PY\_DEV flag to `Y' for compilation. This will alter a subset of the code to be more suitable for gt4py in the python port. When we have all the features we need on the python side and are confident in the robustness of the new model, we should be able to remove these.  The original FV3 sources will be mounted to `/FV3/original`. To compile the model for serialization inside the container, you can use the commands

```bash
cd /FV3
make clean
make -C original serialize_preprocess
make -j8
```

If you need to be able to see the preprocessed `atmos_cubed_sphere` sources on your
host filesystem, you can do that with:

```bash
OTHER_MOUNTS="-v <local dir>:/FV3/atmos_cubed_sphere" make enter_serialize
```

Where `<local_dir>` is replaced with the directory on your host filesystem where you
want to see the preprocessed sources.

This workflow does not support viewing preprocessed sources in the root `/FV3` directory
on the host filesystem, because `/FV3` has a bind-mounted subdirectory `original` and
it is not possible to contain a bind-mounted subdirectory in a separately bind-mounted
directory. You could see these sources by setting `SERIALBOX_OUTDIR` to a different
directory (it is `/FV3` by default) that you have bind-mounted in to the container.

# The Python wrapper

The `FV3/wrapper` subdirectory contains a python wrappper that can be used to
call the fortran model for interactive ML. It's usage docs can be seen [here](TODO/add/path).

To build the wrapper some additional python requirements must be available in the environment:
- fv3config
- pace-util
- numpy
- pyyaml
- xarray
- cython
- mpi4py

The [nix environment][#developing] is setup automatically with these
dependencies, and is the recommend development environment for iterative
development (edit/build/test) of the wrapper. In other environments, e.g.
HPC/DOCKER these builds dependencies will need to be manually installed.

Once the dependencies are installed the wrapper and fv3.exe can be built like
this

    make -C FV3 wrapper_build

To install the wrapper in some python environment, you can use the wheel built
above

    pip install FV3/wrapper/dist/fv3gfs_wrapper*.whl

To test the wrapper

    # if you want to test FV3/wrapper in-place uncomment the line below
    # otherwise it will use the version installed in the python environment
    # export PYTHONPATH=$(pwd)/FV3/wrapper:$PYTHONPATH

    make test_wrapper

# Docker BuildKit

If you are using a version of docker that supports it, you can enable buildkit by
setting `DOCKER_BUILDKIT=1` as an environment variable. This can be useful when building docker targets in this repo, because it will avoid building mulit-stage targets that are not required for the final image.

