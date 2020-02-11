GCR_URL = us.gcr.io/vcm-ml
DOCKERFILE ?= docker/Dockerfile
COMPILED_TAG_NAME ?= latest
ENVIRONMENT_TAG_NAME ?= latest
COMPILE_OPTION ?=
COMPILE_TARGET ?= fv3gfs-compiled
ENVIRONMENT_TARGET ?= fv3gfs-environment
COMPILED_IMAGE ?= $(GCR_URL)/$(COMPILE_TARGET):$(COMPILED_TAG_NAME)
SERIALIZE_IMAGE ?= $(GCR_URL)/$(COMPILE_TARGET):$(COMPILED_TAG_NAME)-serialize
ENVIRONMENT_IMAGE=$(GCR_URL)/$(ENVIRONMENT_TARGET):$(ENVIRONMENT_TAG_NAME)
IMAGE ?= $(ENVIRONMENT_IMAGE)

MOUNTS?=-v $(shell pwd)/FV3:/FV3 \
	-v $(shell pwd)/FV3/conf/configure.fv3.gnu_docker:/FV3/conf/configure.fv3

MOUNTS_SERIALIZE?=-v $(shell pwd)/FV3:/FV3/original

EXPERIMENT ?= new
RUNDIR_CONTAINER=/rundir
RUNDIR_HOST=$(shell pwd)/rundir

build: build_compiled

build_environment:
	docker build -f $(DOCKERFILE) -t $(ENVIRONMENT_IMAGE) \
	--target $(ENVIRONMENT_TARGET) .

build_compiled:
	docker build \
		--build-arg compile_option=$(COMPILE_OPTION) \
		-f $(DOCKERFILE) \
		-t $(COMPILED_IMAGE) \
		--target $(COMPILE_TARGET) .

build_serialize:
	docker build \
		--build-arg compile_option=$(COMPILE_OPTION) \
		--build-arg serialize=true \
		-f $(DOCKERFILE) \
		-t $(SERIALIZE_IMAGE) \
		--target $(COMPILE_TARGET) .

build_debug: build_environment
	COMPILED_TAG_NAME=debug COMPILE_OPTION="REPRO=\\\nDEBUG=Y" $(MAKE) build

enter:
	docker run --rm $(MOUNTS) -w /FV3 -it $(COMPILED_IMAGE) bash

enter_serialize:
	MOUNTS="$(MOUNTS_SERIALIZE)" COMPILED_IMAGE="$(SERIALIZE_IMAGE)" $(MAKE) enter

compile_dev: build_compiled
	docker run --rm $(MOUNTS) -w /FV3 -it $(COMPILED_IMAGE) bash -c "make libs && make fv3.exe"

test:
	pytest tests/pytest --capture=no --verbose --refdir $(shell pwd)/tests/pytest/reference/circleci --image_version $(COMPILED_TAG_NAME)

update_circleci_reference: test
	cd tests/pytest && bash set_reference.sh $(COMPILED_TAG_NAME)-serialize $(shell pwd)/reference/circleci

# 32bit options don't currently build, fix these when issue #4 is fixed.
#test_32bit:
#	COMPILED_TAG_NAME=32bit $(MAKE) test
#
#build_32bit: build_environment
#	COMPILED_TAG_NAME=32bit COMPILE_OPTION=32BIT=Y $(MAKE) build

dev_serialize: # TODO: use run_docker -- string form of command is not translating correctly
	docker run -w=/FV3 \
		-v $(RUNDIR_HOST):/rundir \
		-v $(FV3CONFIG_CACHE_DIR):$(FV3CONFIG_CACHE_DIR) \
		-it $(GCR_URL)/$(COMPILE_TARGET):serialize /bin/bash -c 'cd /FV3;make serialize_preprocess && cd /Serialize/FV3 && make build_serializer && cd /rundir && rm -f Gen*.dat && rm -f *.json &&  /rundir/submit_job.sh /Serialize/'

clean:
	(cd FV3            && make clean)
	$(RM) -f inputdata
	$(RM) -rf tests/pytest/output/*


.PHONY: build build_environment build_compiled enter enter_serialize run test test_32bit clean \
	run_serialize test_serialize test_serialize_image dev_serialize build_serialize \
	build_environment_serialize
