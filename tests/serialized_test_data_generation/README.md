# Generating Serialized Data from FV3GFS

This README assumes that you are in the `./tests/serialized_test_data_generation` directory when executing shell commands.

## Prerequisites

Generating serialize data requires the `fv3config` tool to create run directories from `<experiment>.yml` files. To install `fv3config` you can type the following commands:
```bash
python3 -m venv venv
source venv/bin/activate
pip install --upgrade pip
pip install -r ../../requirements.txt
```

## Quickstart

To build an Docker image with the model compiled for serialization and generate datasets for all
experiments present in the `configs` directory and push the data to the cloud storage bucket, you can simly run
```bash
./make_all_datasets.sh
```
Note that this will take a long time and will make all data on your local hard-drive. Make sure you have enough free space available.
Data will show up in the `data` directory. Model run output will be generated in the `rundir`. Use `make distclean` to remove all data.

Other make targets are available and a quick documentation can be accessed using `make help`.

## Develop a new experiment

In order to develop and test a new experiment for serializing data from a FV3GFS model run, you can use the following workflow

1. In case you need to modify the Fortran code (e.g. add or remove `!$ser` statements or fix a bug), do so in the top-level directory
and using the `Makefile` there. The target you will probably use are `make build_serialize` to test compiling the model in a Docker image
and `make enter_serialize` for interactive development and testing in the running container.

2. Once you are satisfied with your modifications, return to the serialization directory. If you expect the serialized data to change, make sure
you increase the `FORTRAN_VERSION` number in the `Makefile`. You can also use `export FORTRAN_VERSION=exp-7.1.1` to override the value in the Makefile.

3. Build the Fortran image using `make build_model`. If this is the first time you are building the image and Docker can not use the build cache, this may take up more than 15 minutes.

4. Use `export EXPERIMENT=<experiment>` to set the experiment file you want to work with. The file `configs/<experiment>.yml` must exist for the following steps to work.

5. pick the category of savepoints you want to save, from options of 'dycore', 'physics', 'init', 'driver'. These change which savepoints are written out in the runtime. Set your choice with `export ENVS_OVERRIDE=( 'dycore' )` for example. This must be a list with no commas. If you don't set this, by default make_all_datasets will choose what set to run cased on the resolution of the experiment. 

6. By default if a dataset is < c200, all the datapoints will be saved, and if larger SER_INPUT_ONLY="true", the model should exit after writing "Driver-In" or "FVDynamics-In", but in practice this may not work as expected yet. If you are not getting the behavior you expect, change the SER_INPUT_ONLY setting in the make_all_datasets.sh script. 

7. You can now simply type `make generate_and_push_data` which will run the model, ensure that the model run has been successful and data has been generated, pack the data and push the data to the cloud storage bucket. Alternatively, you can execute the individual steps...
   - Type `make setup_rundir` to creates a run directory from the experiment file using `fv3config` which contains all input data to execute a FV3GFS model run.
   - Type `make run_model` to run the Docker container with the model using the run directory you have just created. A `data/<experiment>` directory will be populated with the serialized data and some diagnostic output.
   - Type `make pack_data` to pack the `*.dat` files into a single `*.tar.gz` to reduce the number of files that have to be stored on the cloud.
   - Type `make push_data` to push the serialized data to the cloud storage bucket. If the bucket already exists, the push will fail. You either forgot to increase the `FORTRAN_VERSION` number. In case you are sure you want to overwrite existing data on the cloud, you can `export FORCE_PUSH=true` and try again.

