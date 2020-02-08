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

MOUNTS= -v $(shell pwd)/FV3:/FV3 \
	-v $(shell pwd)/FV3/conf/configure.fv3.gnu_docker:/FV3/conf/configure.fv3

EXPERIMENT ?= new
RUNDIR_CONTAINER=/rundir
RUNDIR_HOST=$(shell pwd)/rundir

SERIALBOX_PATH=$(shell pwd)/serialbox2

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

enter: build_compiled
	docker run --rm $(MOUNTS) -w /FV3 -it $(COMPILED_IMAGE) bash

compile_dev: build_compiled
	docker run --rm $(MOUNTS) -w /FV3 -it $(COMPILED_IMAGE) bash -c "make libs && make fv3.exe"

test:
	pytest tests/pytest --capture=no --verbose --refdir $(shell pwd)/tests/pytest/reference/circleci --image_version $(COMPILED_TAG_NAME)

update_circleci_reference: test
	cd tests/pytest && bash tests/pytest/set_reference.sh circleci

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

run_serialize:
	if [ ! -d "$(RUNDIR_HOST)" ];then \
		echo 'Follow the README.md to setup a run directory';\
	fi
	rm -f $(RUNDIR_HOST)/Gen*.dat
	rm -f $(RUNDIR_HOST)/Archive*.json
	rm -f $(RUNDIR_HOST)/Meta*.json
	./run_docker.sh  $(GCR_URL)/$(COMPILE_TARGET):serialize $(RUNDIR_HOST) $(FV3CONFIG_CACHE_DIR) $(RUNDIR_CONTAINER)/submit_job.sh /Serialize/


clean:
	(cd FV3            && make clean)
	$(RM) -f inputdata
	$(RM) -rf tests/pytest/output/*


.PHONY: build build_environment build_compiled enter run test test_32bit clean \
	run_serialize test_serialize test_serialize_image dev_serialize build_serialize \
	build_environment_serialize
