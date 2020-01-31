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
RUNDIR_CONTAINER=/rundir
RUNDIR_HOST=$(shell pwd)/rundir

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


compile_dev: build_compiled
	docker run --rm $(MOUNTS) -w /FV3 -it $(COMPILED_IMAGE) bash -c "make libs && make fv3.exe"


test:
	pytest tests/pytest -s --refdir $(shell pwd)/tests/pytest/reference/circleci

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
	ENVIRONMENT_TAG_NAME=serialize \
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

dev_serialize: # TODO: use run_docker -- string form of command is not translating correctly
	docker run -w=/FV3 \
		-v $(RUNDIR_HOST):/rundir \
		-v $(FV3CONFIG_CACHE_DIR):$(FV3CONFIG_CACHE_DIR) \
		-it $(GCR_URL)/$(COMPILE_TARGET):serialize /bin/bash -c 'cd /FV3;make serialize_preprocess && cd /Serialize/FV3 && make build_serializer && cd /rundir && rm -f Gen*.dat && rm -f *.json &&  /rundir/submit_job.sh /Serialize/'

# This tests the serialized image can run the serialized code and match the regression
test_serialize:
	pytest tests/pytest -s --image_tag serialize --code_root /Serialize --refdir $(shell pwd)/tests/pytest/reference/circleci_serialize

# This tests the serialized image can run the non-serialized code and match the regression
test_serialize_image:
	pytest tests/pytest -s --image_tag serialize  --refdir $(shell pwd)/tests/pytest/reference/circleci
	$(MAKE) test_serialized

run_serialize:
	if [ ! -d "$(RUNDIR_HOST)" ];then \
		echo 'Follow the README.md to setup a run directory';\
	fi
	rm -f $(RUNDIR_HOST)/Gen*.dat
	rm -f $(RUNDIR_HOST)/Archive*.json
	rm -f $(RUNDIR_HOST)/Meta*.json
	./run_docker.sh  $(GCR_URL)/$(COMPILE_TARGET):serialize $(RUNDIR_HOST) $(FV3CONFIG_CACHE_DIR) $(RUNDIR_CONTAINER)/submit_job.sh /Serialize/



.PHONY: build build_environment build_compiled enter run test test_32bit
