# Generating Serialized Data from FV3GFS

This README assumes that you are in the `./tests/serialized_test_data_generation` directory when executing shell commands.

## Prerequisites

Generating serialize data requires the `fv3config` tool to create run directories from `<configuration>.yml` files. To install `fv3config` you can type the following commands:
```bash
python3 -m venv venv
source venv/bin/activate
pip install --upgrade pip
pip install -r ../../requirements.txt
```

## Quickstart

To build an Docker image with the model compiled for serialization and generate datasets for all
configurations present in the `configs` directory and push the data to the cloud storage bucket, you can simly run
```bash
./make_all_datasets.sh
```
Note that this will take a long time and will make all data on your local hard-drive. Make sure you have enough free space available.
Data will show up in the `data` directory. Model run output will be generated in the `rundir`. Use `make distclean` to remove all data.

Other make targets are available and a quick documentation can be accessed using `make help`.

## Develop a new configuration

In order to develop and test a new configuration for serializing data from a FV3GFS model run, you can use the following workflow

1. In case you need to modify the Fortran code (e.g. add or remove `!$ser` statements or fix a bug), do so in the top-level directory
and using the `Makefile` there. The target you will probably use are `make build_serialize` to test compiling the model in a Docker image
and `make enter_serialize` for interactive development and testing in the running container.

2. Once you are satisfied with your modifications, return to the serialization directory. If you expect the serialized data to change, make sure
you increase the `FORTRAN_VERSION` number in the `Makefile`. You can also use `export FORTRAN_VERSION=exp-7.1.1` to override the value in the Makefile.

3. Build the Fortran image using `make build_model`. If this is the first time you are building the image and Docker can not use the build cache, this may take up more than 15 minutes.

4. Use `export CONFIGURATION=<configuration>` to set the configuration file you want to work with. The file `configs/<configuration>.yml` must exist for the following steps to work.

5. You can now simply type `make generate_and_push_data` which will run the model, ensure that the model run has been successful and data has been generated, pack the data and push the data to the cloud storage bucket. Alternatively, you can execute the individual steps...

5a. Type `make setup_rundir` to creates a run directory from the configuration file using `fv3config` which contains all input data to execute a FV3GFS model run.

5b. Type `make run_model` to run the Docker container with the model using the run directory you have just created. A `data/<configuration>` directory will be populated with the serialized data and some diagnostic output.

5c. Type `make pack_data` to pack the `*.dat` files into a single `*.tar.gz` to reduce the number of files that have to be stored on the cloud.

5d. Type `make push_data` to push the serialized data to the cloud storage bucket. If the bucket already exists, the push will fail. You either forgot to increase the `FORTRAN_VERSION` number. In case you are sure you want to overwrite existing data on the cloud, you can `export FORCE_PUSH=true` and try again.

