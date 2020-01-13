GCR_URL = us.gcr.io/vcm-ml
DOCKERFILE ?= docker/Dockerfile
COMPILED_TAG_NAME ?= default
ENVIRONMENT_TAG_NAME ?=
COMPILE_OPTION ?=

COMPILED_IMAGE ?= $(GCR_URL)/fv3gfs-compiled-$(COMPILED_TAG_NAME)
COMPILE_TARGET ?= fv3gfs-compiled
ENVIRONMENT_TARGET ?= fv3gfs-env
ENVIRONMENT_IMAGE=$(GCR_URL)/fv3gfs-environment$(ENVIRONMENT_TAG_NAME)
IMAGE ?= $(ENVIRONMENT_IMAGE)

MOUNTS= -v $(shell pwd)/FV3:/FV3 \
	-v $(shell pwd)/FV3/conf/configure.fv3.gnu_docker:/FV3/conf/configure.fv3

EXPERIMENT ?= new
RUNDIR_CONTAINER=/FV3/rundir
RUNDIR_HOST=$(shell pwd)/experiments/$(EXPERIMENT)/rundir

SERIALBOX_PATH=$(shell pwd)/serialbox2
SERIALBOX_REPO =git@github.com:VulcanClimateModeling/serialbox2.git

build: build_compiled

build_environment:
	docker build -f $(DOCKERFILE) -t $(ENVIRONMENT_IMAGE) \
	--target $(ENVIRONMENT_TARGET) .

build_compiled: build_environment
	docker build \
		--build-arg compile_option=$(COMPILE_OPTION) \
		-f $(DOCKERFILE) \
		-t $(COMPILED_IMAGE) \
		--target $(COMPILE_TARGET) .

enter: build_environment
	docker run --rm $(MOUNTS) -w /FV3 -it $(IMAGE) bash

compile_dev: build_environment
	docker run --rm $(MOUNTS) -w /FV3 -it $(IMAGE) bash compile $(COMPILE_OPTION)

run_dev: compile_dev
	docker run --rm \
		-v $(RUNDIR_HOST):$(RUNDIR_CONTAINER) \
		-v $(shell pwd)/inputdata/fv3gfs-data-docker/fix.v201702:/inputdata/fix.v201702 \
		-it $(COMPILED_IMAGE) /FV3/rundir/submit_job.sh

build_32bit: build_environment
	COMPILED_TAG_NAME=32bit COMPILE_OPTION=32BIT=Y $(MAKE) build

build_debug: build_environment
	COMPILED_TAG_NAME=debug COMPILE_OPTION="REPRO=\\\nDEBUG=Y" $(MAKE) build

# We might *have* serialized data (pulled from cloud) and not want to rebuild the model...
build_environment_serialize: build_environment
	if [ ! -d "$(SERIALBOX_PATH)" ];then \
		git clone $(SERIALBOX_REPO) $(SERIALBOX_PATH);\
	fi
	ENVIRONMENT_TAG_NAME=:serialize ENVIRONMENT_TARGET=fv3gfs-env-serialize \
	DOCKERFILE=docker/Dockerfile.serialize \
	$(MAKE) build_environment

build_serialize: build_compiled
	if [ ! -d "$(SERIALBOX_PATH)" ];then \
		git clone $(SERIALBOX_REPO) $(SERIALBOX_PATH);\
	fi
	COMPILED_TAG_NAME=serialize COMPILE_TARGET=fv3gfs-compiled-serialize  \
	ENVIRONMENT_TAG_NAME=:serialize ENVIRONMENT_TARGET=fv3gfs-env-serialize \
	DOCKERFILE=docker/Dockerfile.serialize \
	$(MAKE) build

dev_serialize: 
	COMPILED_TAG_NAME=serialize COMPILE_TARGET=fv3gfs-compiled-serialize  \
	ENVIRONMENT_TAG_NAME=:serialize ENVIRONMENT_TARGET=fv3gfs-env-serialize \
	DOCKERFILE=docker/Dockerfile.serialize \
	docker run -w=/FV3 \
		-v $(RUNDIR_HOST):/Serialize/$(RUNDIR_CONTAINER) \
		-v $(shell pwd)/inputdata/fv3gfs-data-docker/fix.v201702:/inputdata/fix.v201702 \
		$(MOUNTS) -it $(GCR_URL)/fv3gfs-compiled-serialize  /bin/bash -c ' make serialize_preprocess ;cd /Serialize/FV3 ; make build_serializer;cd /Serialize/FV3/rundir ; rm -f Gen*.dat; rm -f *.json ;  /Serialize/FV3/rundir/submit_job.sh /Serialize/'


run_serialize: 
	rm -f $(RUNDIR_HOST)/Gen*.dat
	rm -f $(RUNDIR_HOST)/Archive*.json
	rm -f $(RUNDIR_HOST)/Meta*.json
	if [ ! -d $(shell pwd)/inputdata/fv3gfs-data-docker/fix.v201702 ];then\
	    ./download_inputdata.sh ;\
	fi
	docker run --rm \
		-v $(RUNDIR_HOST):/Serialize$(RUNDIR_CONTAINER) \
		-v $(shell pwd)/inputdata/fv3gfs-data-docker/fix.v201702:/inputdata/fix.v201702 \
		-it $(GCR_URL)/fv3gfs-compiled-serialize /Serialize/FV3/rundir/submit_job.sh /Serialize/
run_ser_normal: 
	docker run --rm \
		-v $(RUNDIR_HOST):$(RUNDIR_CONTAINER) \
		-v $(shell pwd)/inputdata/fv3gfs-data-docker/fix.v201702:/inputdata/fix.v201702 \
		-it $(GCR_URL)/fv3gfs-compiled-serialize /FV3/rundir/submit_job.sh

.PHONY: build build_environment build_compiled enter run
