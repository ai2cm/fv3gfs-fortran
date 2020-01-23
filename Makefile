GCR_URL = us.gcr.io/vcm-ml
COMPILED_TAG_NAME ?= default
COMPILE_OPTION ?=

COMPILED_IMAGE ?= $(GCR_URL)/fv3gfs-compiled-$(COMPILED_TAG_NAME)
ENVIRONMENT_IMAGE=$(GCR_URL)/fv3gfs-environment

MOUNTS= -v $(shell pwd)/FV3:/FV3 \
	-v $(shell pwd)/FV3/conf/configure.fv3.gnu_docker:/FV3/conf/configure.fv3

build: build_compiled

build_environment:
	docker build -f docker/Dockerfile -t $(ENVIRONMENT_IMAGE) --target fv3gfs-env .

build_compiled: build_environment
	docker build \
		--build-arg compile_option=$(COMPILE_OPTION) \
		-f docker/Dockerfile \
		-t $(COMPILED_IMAGE) \
		--target fv3gfs-compiled .

enter: build_compiled
	docker run --rm $(MOUNTS) -w /FV3 -it $(COMPILED_IMAGE) bash

compile_dev: build_compiled
	docker run --rm $(MOUNTS) -w /FV3 -it $(COMPILED_IMAGE) bash -c "make libs && make fv3.exe"

test:
	pytest tests/pytest -s --refdir $(pwd)/tests/pytest/reference/circleci

# 32bit options don't currently build, fix these when issue #4 is fixed.
#test_32bit:
#	COMPILED_TAG_NAME=32bit $(MAKE) test
#
#build_32bit: build_environment
#	COMPILED_TAG_NAME=32bit COMPILE_OPTION=32BIT=Y $(MAKE) build

build_debug: build_environment
	COMPILED_TAG_NAME=debug COMPILE_OPTION="REPRO=\\\nDEBUG=Y" $(MAKE) build

.PHONY: build build_environment build_compiled enter run test test_32bit
