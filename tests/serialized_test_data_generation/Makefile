SHELL = /bin/bash

# select configuration file (has to be in ./configs)
CONFIGURATION ?= c12_6ranks_standard

# version number or tag (determines image tag and destination directory in cloud storage)
# -convention: <tag>-<some large conceptual version change>.<serialization statement change>.<hotfix>
# -version number should be increased when serialization data is expected to change
# -omit tag for "operational" serialization data, use tag for experimental serialization data
FORTRAN_VERSION ?= jenkins-7.1.1

# docker container image setup (used for running model, see ../../README.md)
GCR_URL = us.gcr.io/vcm-ml
COMPILED_IMAGE = $(GCR_URL)/fv3gfs-compiled:$(FORTRAN_VERSION)-serialize-gt4pydev
RUN_DIR_CONTAINER = /rundir
DATA_DIR_CONTAINER = $(RUN_DIR_CONTAINER)/test_data

# local host setup
CWD=$(shell pwd)
FORTRAN_DIR = $(CWD)/../../
CONFIGURATION_DIR = $(CWD)/configs
RUN_DIR_HOST = $(CWD)/rundir/$(CONFIGURATION)
DATA_DIR_HOST = $(CWD)/data/$(CONFIGURATION)
TARFILE_HOST = $(DATA_DIR_HOST)/dat_files.tar.gz
TARFILE_INDEX_HOST = $(DATA_DIR_HOST)/tarfile_index.txt
USER_ID_HOST = $(shell id -u)
GROUP_ID_HOST = $(shell id -g)

# cloud storage location for pushing generated data
STORAGE_BUCKET = gs://vcm-fv3gfs-serialized-regression-data
DATA_DIR_BUCKET = $(STORAGE_BUCKET)/$(FORTRAN_VERSION)/$(CONFIGURATION)

help:
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

build_model: ## build Docker image with compiled model code for serialization
	cd $(FORTRAN_DIR) && \
		$(MAKE) pull_deps && \
		        BUILD_ARGS="$(BUILD_ARGS) --build-arg serialize=true" \
			COMPILED_IMAGE=$(COMPILED_IMAGE) \
			COMPILE_OPTION="GT4PY_DEV=Y" \
			$(MAKE) build_compiled

setup_rundir: ## create run directory on local host using fv3config
	@if ! command -v write_run_directory ; then \
		echo "Error: Please install fv3config (e.g. pip install -r ../../requirements.txt)" ; \
		exit 1 ; \
	fi
	@if [ -d $(RUN_DIR_HOST) ] ; then \
		echo "Error: Run directory $(RUN_DIR_HOST) already exists. Run make clean?" ; \
		exit 1 ; \
	fi
	write_run_directory $(CONFIGURATION_DIR)/$(CONFIGURATION).yml $(RUN_DIR_HOST)
	cp submit_job.sh $(RUN_DIR_HOST)
	cp count_ranks.py $(RUN_DIR_HOST)
	git rev-parse HEAD > $(RUN_DIR_HOST)/fortran_sha.txt

run_model: ## run model using Docker image to generate serialized data
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
		--network host --rm -v $(RUN_DIR_HOST):$(RUN_DIR_CONTAINER) \
		-v $(DATA_DIR_HOST):$(DATA_DIR_CONTAINER) $(COMPILED_IMAGE) \
		/rundir/submit_job.sh
	cd $(DATA_DIR_HOST) && \
		md5sum * > md5sums.txt
	./check_data_dir.sh $(DATA_DIR_HOST)

pack_data: ## pack *.dat files into a *.tar.gz and delete them
	@if [ -f $(TARFILE_HOST) ] ; then \
		echo "Error: Tar file $(TARFILE_HOST) already exists. Already packed?" ; \
		exit 1 ; \
	fi
	cd $(DATA_DIR_HOST) && \
		tar -cvzf $(TARFILE_HOST) *.dat > $(TARFILE_INDEX_HOST) && \
		rm -f *.dat ; \

unpack_data: ## unack *.dat files from a *.tar.gz and remove tar-file
	@if [ ! -f $(TARFILE_HOST) ] ; then \
		echo "Error: Tar file $(TARFILE_HOST) does not exist. Already unpacked?" ; \
		exit 1 ; \
	fi
	cd $(DATA_DIR_HOST) && \
		tar -xvf $(TARFILE_HOST) && \
		rm -f $(TARFILE_HOST) $(TARFILE_INDEX_HOST)  ; \

push_data: ## push packed data to cloud storage bucket
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

clean: ## remove artifacts for current configuration
	rm -rf $(DATA_DIR_HOST)
	rm -rf $(RUN_DIR_HOST)

distclean: ## remove all artifacts
	rm -rf $(CWD)/rundir
	rm -rf $(CWD)/data

generate_data: build_model setup_rundir run_model ## build, setup, and run for a specific configuration

generate_and_push_data: generate_data pack_data push_data ## build, setup, run, pack and push for a specific configuration

.PHONY: build_model setup_rundir run_model pack_data \
	unpack_data push_data generate_and_push_data

