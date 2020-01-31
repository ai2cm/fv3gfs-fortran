GCR_URL = us.gcr.io/vcm-ml
DOCKERFILE ?= docker/Dockerfile
COMPILED_TAG_NAME ?= default
ENVIRONMENT_TAG_NAME ?= latest
COMPILE_OPTION ?=
COMPILE_TARGET ?= fv3gfs-compiled
ENVIRONMENT_TARGET ?= fv3gfs-environment
COMPILED_IMAGE ?= $(GCR_URL)/$(COMPILE_TARGET):$(COMPILED_TAG_NAME)


ENVIRONMENT_IMAGE=$(GCR_URL)/$(ENVIRONMENT_TARGET):$(ENVIRONMENT_TAG_NAME)
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

enter: build_compiled
	docker run --rm $(MOUNTS) -w /FV3 -it $(COMPILED_IMAGE) bash

# TODO: remove?
run_dev: compile_dev
	docker run --rm \
		-v $(RUNDIR_HOST):$(RUNDIR_CONTAINER) \
		-v $(shell pwd)/inputdata/fv3gfs-data-docker/fix.v201702:/inputdata/fix.v201702 \
		-it $(COMPILED_IMAGE) /FV3/rundir/submit_job.sh

compile_dev: build_compiled
	docker run --rm $(MOUNTS) -w /FV3 -it $(COMPILED_IMAGE) bash -c "make libs && make fv3.exe"


test:
	pytest tests/pytest -s --refdir $(pwd)/tests/pytest/reference/circleci

update_circleci_reference: test
	cd tests/pytest && bash tests/pytest/set_reference.sh circleci

# 32bit options don't currently build, fix these when issue #4 is fixed.
#test_32bit:
#	COMPILED_TAG_NAME=32bit $(MAKE) test
#
#build_32bit: build_environment
#	COMPILED_TAG_NAME=32bit COMPILE_OPTION=32BIT=Y $(MAKE) build

build_debug: build_environment
	COMPILED_TAG_NAME=debug COMPILE_OPTION="REPRO=\\\nDEBUG=Y" $(MAKE) build

# We might *have* serialized data (pulled from cloud) and not want to rebuild the model...
build_environment_serialize: build_environment
	if [ ! -d "$(SERIALBOX_PATH)" ];then \
		git clone $(SERIALBOX_REPO) $(SERIALBOX_PATH);\
	fi
	ENVIRONMENT_TAG_NAME=:serialize \
	DOCKERFILE=docker/Dockerfile.serialize \
	$(MAKE) build_environment

build_serialize: build_compiled
	if [ ! -d "$(SERIALBOX_PATH)" ];then \
		git clone $(SERIALBOX_REPO) $(SERIALBOX_PATH);\
	fi
	COMPILED_TAG_NAME=serialize  \
	ENVIRONMENT_TAG_NAME=serialize \
	DOCKERFILE=docker/Dockerfile.serialize \
	$(MAKE) build

dev_serialize: 
	COMPILED_TAG_NAME=serialize  \
	ENVIRONMENT_TAG_NAME=serialize  \
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
		-it $(GCR_URL)/$(COMPILE_TARGET):serialize /Serialize/FV3/rundir/submit_job.sh /Serialize/
run_ser_normal: 
	docker run --rm \
		-v $(RUNDIR_HOST):$(RUNDIR_CONTAINER) \
		-v $(shell pwd)/inputdata/fv3gfs-data-docker/fix.v201702:/inputdata/fix.v201702 \
		-it $(GCR_URL)/$(COMPILE_TARGET):serialize /FV3/rundir/submit_job.sh

.PHONY: build build_environment build_compiled enter run test test_32bit
