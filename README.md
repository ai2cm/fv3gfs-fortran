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
│   ├── atmos_cubed_sphere # https://github.com/NOAA-GFDL/GFDL_atmos_cubed_sphere
│   └── ccpp
│       ├── framework      # https://github.com/NCAR/ccpp-framework
│       └── physics        # https://github.com/NCAR/ccpp-physics
├── serialbox              # https://github.com/GridTools/serialbox
└── stochastic_physics     # https://github.com/noaa-psd/stochastic_physics
```

In some cases these are actual submodules, and in other cases they are
subtrees.

## Developing the model and running tests

### Nix

The primary mode of development in this repository now runs through the
[nix](https://nixos.org/) package manager.  This package manager is available on
Mac and Linux, and provides a light weight means to distribute isolated software
environments. 

To begin, install nix following [these
instructions](https://nixos.org/download.html).

(optional) We host binaries using a tool called cachix, and this will greatly
speed up any builds. To use our binaries, [install
cachix](https://github.com/cachix/cachix#installation) and then run

    cachix use vulcanclimatemodeling

Without using the cachix cache, FV3 and all its dependencies will need to build
from source (~20 minutes). This only happens once per machine, but it is slow.

#### Installation

To begin, install nix following [these instructions](https://nixos.org/download.html).

(optional) We host binaries using a tool called cachix, and this will greatly
speed up any builds. To use our binaries, [install
cachix](https://github.com/cachix/cachix#installation) and then run

    cachix use vulcanclimatemodeling

Without using the cachix cache, FV3 and all its dependencies will need to build
from source (~20 minutes). This only happens once per machine, but it is slow.

#### Developing

To develop the model, you can use the environment specified in `shell.nix` by running

    nix-shell

Then configure the build to use nix

    cd FV3 && ./configure nix

And build the model (from the root directory)

    make build_repro
    
And the wrapper

    make build_wrapper

At this point you can run the tests.

## Testing the fortran model

Tests of the pure fortran model are included in the `tests` subdirectory.  You
can see which ones run in continuous integration by inspecting
`.circleci/config.yml`.

Regression tests which check the bit-reproducibility of results are
performed for a set of reference configurations included in `tests/pytest/config`.
Please read the README in `tests/pytest` for more information about these regression
tests and how to update the reference checksums.

### Testing the model in different compile modes

The fortran model can be built with different sets of compiler flags, defined in
the `FV3/conf/*.configure.fv3` files.  These sets of compiler flags facilitate
running the model in different modes.  For production workflows, we run the
model with the `repro` set of compiler flags; for debugging it can be useful to
compile and run the model with the `debug` set of compiler flags, which can
facilitate catching array out of bounds errors and the like.  

To build the model in `repro` mode use:

    make build_repro

To build the model in `debug` mode use:

    make build_debug

To build executables in both `repro` and `debug` mode use:

    make build

The test infrastructure is written to detect and run tests only for which the
appropriate executable exists, since regression tests produce different results
when running in `repro` versus `debug` mode.

To run the full suite of fortran model tests for the compiled executables, use:

    make test_fortran

Since the tests are time-consuming, during development it can often be helpful
to select specific tests that pertain to the code you are developing, e.g. with
the `-k` option of `pytest`:

    pytest -v -k test_use_prescribed_sea_surface_properties tests/pytest/test_regression.py

There are also make rules which select tests to run based on `pytest` marks,
e.g.:

    make test_fortran_basic

## Testing the Python wrapper

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

Once the dependencies are installed, including the `repro`-mode version of the
fortran executable, the wrapper can be built like this

    make build_wrapper

To install the wrapper in some python environment, you can use the wheel built
above

    pip install FV3/wrapper/dist/fv3gfs_wrapper*.whl

To test the wrapper

    # if you want to test FV3/wrapper in-place uncomment the line below
    # otherwise it will use the version installed in the python environment
    # export PYTHONPATH=$(pwd)/FV3/wrapper:$PYTHONPATH

    make test_wrapper

## Authentication

The input data files required by our tests are stored in a GCS bucket, stored in
the `us-central1` region. This data is free to use, but we have enabled
requestor-pays to avoid paying for network transfer costs incurred by external
users. You will need to authenticate with your own google cloud project
credentials to access this data. Detailed instructions are out of scope, but
usually involves the setting the following environmental variables
```
export FSSPEC_GS_REQUESTER_PAYS="on"
export GOOGLE_APPLICATION_CREDENTIALS=/path/to/key.json
```
For more information see [this
documentation](https://gcsfs.readthedocs.io/en/latest/api.html#gcsfs.core.GCSFileSystem).
