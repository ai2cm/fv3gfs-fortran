SHELL = /bin/bash

# select experiment directory and file
EXPERIMENT_DIR = $(CWD)/configs
EXPERIMENT ?= c12_6ranks_standard
# base images w/ or w/o CUDA
CUDA ?=y
SER_ENV ?= dycore
# version number or tag (determines image tag and destination directory in cloud storage)
# -convention: <tag>-<some large conceptual version change>.<serialization statement change>.<hotfix>
# -version number should be increased when serialization data is expected to change
# -omit tag (e.g. 7.1.1) for "operational" serialization data, use tag for experimental serialization data
FORTRAN_VERSION ?= 8.1.1
# docker container image setup (used for running model, see ../../README.md)
GCR_URL = us.gcr.io/vcm-ml
COMPILED_IMAGE = $(GCR_URL)/fv3gfs-compiled:gnu7-mpich314-nocuda-serialize
RUN_DIR_CONTAINER = /rundir
DATA_DIR_CONTAINER = /data

# local host setup
CWD=$(shell pwd)
FORTRAN_DIR = $(CWD)/../../
RUN_DIR_HOST = $(CWD)/rundir/$(EXPERIMENT)/$(SER_ENV)
DATA_DIR_HOST = $(CWD)/data/$(EXPERIMENT)/$(SER_ENV)
TARFILE_HOST = $(DATA_DIR_HOST)/dat_files.tar.gz
TARFILE_INDEX_HOST = $(DATA_DIR_HOST)/tarfile_index.txt
USER_ID_HOST = $(shell id -u)
GROUP_ID_HOST = $(shell id -g)

# cloud storage location for pushing generated data
STORAGE_BUCKET = gs://vcm-fv3gfs-serialized-regression-data
DATA_DIR_BUCKET = $(STORAGE_BUCKET)/$(FORTRAN_VERSION)/$(EXPERIMENT)/$(SER_ENV)

help:
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

build_model: ## build Docker image with compiled model code for serialization
	@echo ""
	@echo ">> Building Docker image $(COMPILED_IMAGE)"
	cd $(FORTRAN_DIR) && \
		BUILD_ARGS="$(BUILD_ARGS) --build-arg serialize=true" \
		COMPILED_IMAGE=$(COMPILED_IMAGE) \
		COMPILE_OPTION="GT4PY_DEV=Y\\\nAI2_SUBSET_PHYSICS=Y" \
		$(MAKE) build_compiled

setup_rundir: ## create run directory on local host using fv3config
	@echo ""
	@echo ">> Generating run directory $(RUN_DIR_HOST)"
	@if ! command -v write_run_directory ; then \
		echo "Error: Please install fv3config (e.g. pip install -r ../../requirements.txt)" ; \
		exit 1 ; \
	fi
	@if [ -d $(RUN_DIR_HOST) ] ; then \
		echo "Error: Run directory $(RUN_DIR_HOST) already exists. Run make clean?" ; \
		exit 1 ; \
	fi
	write_run_directory $(EXPERIMENT_DIR)/$(EXPERIMENT).yml $(RUN_DIR_HOST)
	cp submit_job.sh $(RUN_DIR_HOST)
	cp count_ranks.py $(RUN_DIR_HOST)
	git rev-parse HEAD > $(RUN_DIR_HOST)/fortran_sha.txt

run_model: ## run model using Docker image to generate serialized data
	@echo ""
	@echo ">> Running model using image $(COMPILED_IMAGE) in run directory $(DATA_DIR_HOST)"
	@if [ ! "`docker image ls $(COMPILED_IMAGE) | wc -l`" -eq 2 ] ; then \
		echo "Error: Docker image $(COMPILE_IMAGE) does not exist. Run make build_model?" ; \
		exit 1 ; \
	fi
	@if [ -d $(DATA_DIR_HOST) ] ; then \
		echo "Error: Data directory $(DATA_DIR_HOST) already exists. Run make clean?" ; \
		exit 1 ; \
	fi
	mkdir -p $(DATA_DIR_HOST)
	docker run -e USER_ID_HOST=$(USER_ID_HOST) -e GROUP_ID_HOST=$(GROUP_ID_HOST) \
		-e SER_ENV=$(SER_ENV) -e SAVE_TIMESTEP=$(SAVE_TIMESTEP) \
		--network host --rm -v $(RUN_DIR_HOST):$(RUN_DIR_CONTAINER) \
		-v $(DATA_DIR_HOST):$(DATA_DIR_CONTAINER) \
                --mount type=tmpfs,destination=$(RUN_DIR_CONTAINER)/test_data \
                --shm-size=512m \
		$(COMPILED_IMAGE) /rundir/submit_job.sh
	cd $(DATA_DIR_HOST) && \
		find . -type f \( -name "*.dat" -o -name "*.nml" -o -name "*.json" \) -exec md5sum {} \; > md5sums.txt
	./check_data_dir.sh $(DATA_DIR_HOST)
	cp $(EXPERIMENT_DIR)/$(EXPERIMENT).yml $(DATA_DIR_HOST)
	cp $(EXPERIMENT_DIR)/$(EXPERIMENT).yml $(DATA_DIR_HOST)/input.yml

pack_data: ## convert to netcdf files, pack *.dat files into a *.tar.gz and delete them
	@echo ""
	@echo ">> Packing data in $(DATA_DIR_HOST) to $(TARFILE_HOST)"
	@if [ -f $(TARFILE_HOST) ] ; then \
		echo "Error: Tar file $(TARFILE_HOST) already exists. Already packed?" ; \
		exit 1 ; \
	fi
	docker run --mount type=bind,source=$(shell pwd)/serialbox_to_netcdf.py,target=/root/serialbox_to_netcdf.py -v $(DATA_DIR_HOST):/data $(COMPILED_IMAGE) bash -c "python3 /root/serialbox_to_netcdf.py /data /data"
	cd $(DATA_DIR_HOST) && \
		tar -cvzf $(TARFILE_HOST) *.dat > $(TARFILE_INDEX_HOST) && \
		rm -f *.dat ; \

unpack_data: ## unpack *.dat files from a *.tar.gz and remove tar-file
	@echo ""
	@echo ">> Unpacking data from $(TARFILE_HOST) to $(DATA_DIR_HOST)"
	@if [ ! -f $(TARFILE_HOST) ] ; then \
		echo "Error: Tar file $(TARFILE_HOST) does not exist. Already unpacked?" ; \
		exit 1 ; \
	fi
	cd $(DATA_DIR_HOST) && \
		tar -xvf $(TARFILE_HOST) && \
		rm -f $(TARFILE_HOST) $(TARFILE_INDEX_HOST)  ; \

validate_data: ## compare generated data with data stored on cloud storage bucket
	@echo ""
	@echo ">> Validating data generated in $(DATA_DIR_HOST) with cloud data in $(DATA_DIR_BUCKET)"
	@if [ ! gsutil ls $(DATA_DIR_BUCKET) &> /dev/null ] ; then \
		echo "Error: version $(FORTRAN_VERSION) of data does not exist on cloud storage $(DATA_DIR_BUCKET)." ; \
		exit 1 ; \
	fi
	@if [ ! gsutil ls $(DATA_DIR_BUCKET)/md5sums.txt &> /dev/null ] ; then \
		echo "Error: cannot find md5sums.txt on cloud storage $(DATA_DIR_BUCKET)." ; \
		exit 1 ; \
	fi
	@if [ -f $(TARFILE_HOST) ] ; then \
		echo "Error: Tar file $(TARFILE_HOST) exists. You need to unpack for validation." ; \
		exit 1 ; \
	fi
	gsutil cp $(DATA_DIR_BUCKET)/md5sums.txt /tmp/md5sums.txt
	cd $(DATA_DIR_HOST) && \
		md5sum --check --quiet /tmp/md5sums.txt

push_data: ## push packed data to cloud storage bucket
	@echo ""
	@echo ">> Pushing data in $(DATA_DIR_HOST) to cloud storage $(DATA_DIR_BUCKET)"
	@if compgen -G "$(DATA_DIR_HOST)/*.dat" &> /dev/null ; then \
		echo "Error: please run make pack_data before pushing to cloud storage bucket." ; \
		exit 1 ; \
	fi
	@if [ "${FORCE_PUSH}" != "true" ] ; then \
		if gsutil ls $(DATA_DIR_BUCKET) &> /dev/null ; then \
			echo "Error: version $(FORTRAN_VERSION) of data already exists on cloud storage $(DATA_DIR_BUCKET)." ; \
			echo "Note: Increment the FORTRAN_VERSION if you have a new or experimental set of data." ; \
			echo "Note: Use export FORCE_PUSH=true to force a push, if you want to overwrite an existing data set." ; \
			exit 1 ; \
		fi ; \
	fi
	gsutil -m rsync -d -r $(DATA_DIR_HOST) $(DATA_DIR_BUCKET)

pull_data: ## pull packed data from cloud storage bucket
	mkdir -p $(DATA_DIR_HOST)
	gsutil -m rsync -d -r $(DATA_DIR_BUCKET) $(DATA_DIR_HOST)

clean: ## remove artifacts for current experiment
	@echo ""
	@echo ">> Removing $(DATA_DIR_HOST) and $(RUN_DIR_HOST)"
	rm -rf $(DATA_DIR_HOST)
	rm -rf $(RUN_DIR_HOST)

distclean: ## remove all artifacts
	@echo ""
	@echo ">> Removing $(CWD)/rundir and $(CWD)/data"
	rm -rf $(CWD)/rundir
	rm -rf $(CWD)/data

generate_data: build_model setup_rundir run_model ## build, setup, and run for a specific experiment

.PHONY: build_model setup_rundir run_model pack_data \
	unpack_data push_data generate_data validate_data \
	pack_and_push_data

