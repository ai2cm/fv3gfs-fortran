# setup (use XXX=<value> make <target> to override)
GCR_URL ?= us.gcr.io/vcm-ml
DOCKERFILE ?= docker/Dockerfile
COMPILED_TAG_NAME ?= latest
ENVIRONMENT_TAG_NAME ?= latest
COMPILE_OPTION ?=
COMPILE_TARGET ?= fv3gfs-compiled
BUILD_ARGS ?=
BUILD_FROM_INTERMEDIATE ?= n
ENVIRONMENT_TARGET ?= fv3gfs-environment
CUDA ?= n
OTHER_MOUNTS ?= 

# image names (use XXX_IMAGE=<name> make <target> to override)
COMPILED_IMAGE ?= $(GCR_URL)/$(COMPILE_TARGET):$(COMPILED_TAG_NAME)
SERIALIZE_IMAGE ?= $(GCR_URL)/$(COMPILE_TARGET):$(COMPILED_TAG_NAME)-serialize
ENVIRONMENT_IMAGE ?= $(GCR_URL)/$(ENVIRONMENT_TARGET):$(ENVIRONMENT_TAG_NAME)
MPI_IMAGE ?= $(GCR_URL)/mpi-build:$(DEP_TAG_NAME)
FMS_IMAGE ?= $(GCR_URL)/fms-build:$(DEP_TAG_NAME)
ESMF_IMAGE ?= $(GCR_URL)/esmf-build:$(DEP_TAG_NAME)
SERIALBOX_IMAGE ?= $(GCR_URL)/serialbox-build:$(DEP_TAG_NAME)

# base images w/ or w/o CUDA
ifeq ($(CUDA),n)
	BASE_IMAGE ?= ubuntu:19.10
	DEP_TAG_NAME ?= gnu9-mpich314-nocuda
else
	BASE_IMAGE ?= nvidia/cuda:10.2-devel-ubuntu18.04
	DEP_TAG_NAME ?= gnu8-mpich314-cuda102
endif
BUILD_ARGS += --build-arg BASE_IMAGE=$(BASE_IMAGE)

# used to shorten build times in CircleCI
ifeq ($(BUILD_FROM_INTERMEDIATE),y)
	BUILD_ARGS += --build-arg FMS_IMAGE=$(FMS_IMAGE) --build-arg ESMF_IMAGE=$(ESMF_IMAGE) --build-arg SERIALBOX_IMAGE=$(SERIALBOX_IMAGE) --build-arg MPI_IMAGE=$(MPI_IMAGE)
endif

.PHONY: help build build_environment build_compiled build_serialize build_serialize_gt4py_dev build_debug build_coverage
.PHONY: build_deps push_deps pull_deps enter enter_serialize test update_circleci_reference clean

help:
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

build: build_compiled ## build default container image (production)

build_environment: ## build environment container image
	docker build -f $(DOCKERFILE) -t $(ENVIRONMENT_IMAGE) \
	--target $(ENVIRONMENT_TARGET) .

build_compiled: ## build production container image
	docker build \
		--build-arg compile_option=$(COMPILE_OPTION) \
		$(BUILD_ARGS) \
		-f $(DOCKERFILE) \
		-t $(COMPILED_IMAGE) \
		--target $(COMPILE_TARGET) .

build_serialize:
	BUILD_ARGS="$(BUILD_ARGS) --build-arg serialize=true" \
	COMPILED_IMAGE=$(SERIALIZE_IMAGE) \
	$(MAKE) build_compiled

build_serialize_gt4py_dev: ## build container image for generating serialized data
	BUILD_ARGS="$(BUILD_ARGS) --build-arg serialize=true" \
		COMPILED_IMAGE=$(SERIALIZE_IMAGE)-gt4pydev \
		COMPILE_OPTION="GT4PY_DEV=Y" \
		$(MAKE) build_compiled

build_debug: ## build container image for debugging
	COMPILED_TAG_NAME=debug COMPILE_OPTION="REPRO=\\\nDEBUG=Y" $(MAKE) build

build_coverage: build_environment ## build container image for code coverage analysis
	COMPILED_TAG_NAME=gcov COMPILED_IMAGE=$(GCR_URL)/$(COMPILE_TARGET):gcov \
	COMPILE_OPTION="OPENMP=\\\nREPRO=\\\nDEBUG=Y\\\nGCOV=Y" $(MAKE) build

build_deps: ## build container images of dependnecies (FMS, ESMF, SerialBox)
	docker build -f $(DOCKERFILE) -t $(MPI_IMAGE) $(BUILD_ARGS) --target fv3gfs-mpi .
	docker build -f $(DOCKERFILE) -t $(FMS_IMAGE) $(BUILD_ARGS) --target fv3gfs-fms .
	docker build -f $(DOCKERFILE) -t $(ESMF_IMAGE) $(BUILD_ARGS) --target fv3gfs-esmf .
	docker build -f $(DOCKERFILE) -t $(SERIALBOX_IMAGE) $(BUILD_ARGS) --target fv3gfs-environment-serialbox .

push_deps: ## push container images of dependencies to GCP
	docker push $(MPI_IMAGE)
	docker push $(FMS_IMAGE)
	docker push $(ESMF_IMAGE)
	docker push $(SERIALBOX_IMAGE)

pull_deps: ## pull container images of dependnecies from GCP (for faster builds)
	docker pull $(MPI_IMAGE)
	docker pull $(FMS_IMAGE)
	docker pull $(ESMF_IMAGE)
	docker pull $(SERIALBOX_IMAGE)

enter: ## run and enter production container for development
	docker run --rm \
		-v $(shell pwd)/FV3:/FV3 $(OTHER_MOUNTS) \
		-w /FV3 -it $(COMPILED_IMAGE) bash

enter_serialize: ## run and enter serialization container for development
	@echo ">>> NOTE: Make sure to compile with GT4PY_DEV=Y when developing serialization"
	docker run --rm \
		-v $(shell pwd)/FV3:/FV3/original $(OTHER_MOUNTS) \
		-w /FV3 -it $(SERIALIZE_IMAGE) bash

test: ## run tests (set COMPILED_TAG_NAME to override default)
	pytest tests/pytest --capture=no --verbose --refdir $(shell pwd)/tests/pytest/reference/circleci --image_version $(COMPILED_TAG_NAME)

update_test_reference: test ## update md5 checksums for regression tests
	cd tests/pytest && bash set_reference.sh $(COMPILED_TAG_NAME)-serialize $(shell pwd)/reference/circleci

clean: ## cleanup source tree and test output
	(cd FV3 && make clean)
	$(RM) -f inputdata
	$(RM) -rf tests/pytest/output/*

# TODO 32bit options don't currently build, fix these when issue #4 is fixed.
#test_32bit:
#	COMPILED_TAG_NAME=32bit $(MAKE) test
#
#build_32bit: build_environment
#	COMPILED_TAG_NAME=32bit COMPILE_OPTION=32BIT=Y $(MAKE) build
#

