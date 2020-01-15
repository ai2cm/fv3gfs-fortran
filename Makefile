GCR_URL = us.gcr.io/vcm-ml
COMPILED_TAG_NAME ?= default
COMPILE_OPTION ?=

COMPILED_IMAGE ?= $(GCR_URL)/fv3gfs-compiled-$(COMPILED_TAG_NAME)
ENVIRONMENT_IMAGE=$(GCR_URL)/fv3gfs-environment
IMAGE ?= $(ENVIRONMENT_IMAGE)

MOUNTS= -v $(shell pwd)/sorc/fv3gfs.fd/FV3:/FV3 \
	-v $(shell pwd)/sorc/fv3gfs.fd/FV3/conf/configure.fv3.gnu_docker:/FV3/conf/configure.fv3

EXPERIMENT ?= new
RUNDIR_CONTAINER=/FV3/rundir
RUNDIR_HOST=$(shell pwd)/experiments/$(EXPERIMENT)/rundir

build: build_compiled

build_environment:
	docker build -f docker/Dockerfile -t $(ENVIRONMENT_IMAGE) --target fv3gfs-env .

build_compiled: build_environment
	docker build \
		--build-arg compile_option=$(COMPILE_OPTION) \
		-f docker/Dockerfile \
		-t $(COMPILED_IMAGE) \
		--target fv3gfs-compiled .

enter: build_environment
	docker run --rm $(MOUNTS) -w /FV3 -it $(IMAGE) bash

compile_dev: build_environment
	docker run --rm $(MOUNTS) -w /FV3 -it $(IMAGE) bash compile $(COMPILE_OPTION)

run_dev: compile_dev
	docker run --rm \
		-v $(RUNDIR_HOST):$(RUNDIR_CONTAINER) \
		-v $(shell pwd)/inputdata/fv3gfs-data-docker/fix.v201702:/inputdata/fix.v201702 \
		-it $(IMAGE) /FV3/rundir/submit_job.sh

test:
	./test_docker.sh $(COMPILED_TAG_NAME)

test_32bit:
	COMPILED_TAG_NAME=32bit $(MAKE) test

build_32bit: build_environment
	COMPILED_TAG_NAME=32bit COMPILE_OPTION=32BIT=Y $(MAKE) build

build_debug: build_environment
	COMPILED_TAG_NAME=debug COMPILE_OPTION="REPRO=\\\nDEBUG=Y" $(MAKE) build

.PHONY: build build_environment build_compiled enter run test test_32bit
