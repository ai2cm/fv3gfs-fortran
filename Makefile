# setup (use XXX=<value> make <target> to override)
GCR_URL ?= us.gcr.io/vcm-ml
COMMIT_SHA := $(shell git rev-parse HEAD)
DOCKERFILE ?= docker/Dockerfile
ENVIRONMENT_TAG_NAME ?= latest
COMPILE_OPTION ?=
COMPILE_TARGET ?= fv3gfs-compiled
BUILD_ARGS ?=
BUILD_FROM_INTERMEDIATE ?= n
ENVIRONMENT_TARGET ?= fv3gfs-environment
CUDA ?= n
OTHER_MOUNTS ?= 

# base images w/ or w/o CUDA
ifeq ($(CUDA),n)
	BASE_IMAGE ?=ubuntu:18.04
	DEP_TAG_NAME ?=gnu7-mpich314-nocuda
else
	BASE_IMAGE ?=nvidia/cuda:10.2-devel-ubuntu18.04
	DEP_TAG_NAME ?=gnu7-mpich314-cuda102
endif
BUILD_ARGS += --build-arg BASE_IMAGE=$(BASE_IMAGE)

# image names (use XXX_IMAGE=<name> make <target> to override)
COMPILED_TAG_NAME ?=$(DEP_TAG_NAME)
COMPILED_IMAGE ?= $(GCR_URL)/$(COMPILE_TARGET):$(COMPILED_TAG_NAME)
SERIALIZE_IMAGE ?= $(GCR_URL)/$(COMPILE_TARGET):$(COMPILED_TAG_NAME)-serialize
EMULATION_IMAGE ?= $(GCR_URL)/$(COMPILE_TARGET):$(COMPILED_TAG_NAME)-emulation
ENVIRONMENT_IMAGE ?= $(GCR_URL)/$(ENVIRONMENT_TARGET):$(ENVIRONMENT_TAG_NAME)
MPI_IMAGE ?= $(GCR_URL)/mpi-build:$(DEP_TAG_NAME)
FMS_IMAGE ?= $(GCR_URL)/fms-build:$(DEP_TAG_NAME)
ESMF_IMAGE ?= $(GCR_URL)/esmf-build:$(DEP_TAG_NAME)
SERIALBOX_IMAGE ?= $(GCR_URL)/serialbox-build:$(DEP_TAG_NAME)

# used to shorten build times in CircleCI
ifeq ($(BUILD_FROM_INTERMEDIATE),y)
	BUILD_ARGS += --build-arg FMS_IMAGE=$(FMS_IMAGE) --build-arg ESMF_IMAGE=$(ESMF_IMAGE) --build-arg SERIALBOX_IMAGE=$(SERIALBOX_IMAGE) --build-arg MPI_IMAGE=$(MPI_IMAGE)
endif

.PHONY: help build build_environment build_compiled build_serialize build_debug
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

build_debug: ## build container image for debugging
	COMPILED_TAG_NAME=$(COMPILED_TAG_NAME)-debug COMPILE_OPTION="REPRO=\\\nDEBUG=Y" $(MAKE) build

build_serialize: ## build container image for serialization
	BUILD_ARGS="$(BUILD_ARGS) --build-arg serialize=true" \
	COMPILED_IMAGE=$(SERIALIZE_IMAGE) \
	$(MAKE) build_compiled

build_deps: ## build container images of dependnecies (FMS, ESMF, SerialBox)
	docker build -f $(DOCKERFILE) -t $(MPI_IMAGE) $(BUILD_ARGS) --target fv3gfs-mpi .
	docker build -f $(DOCKERFILE) -t $(FMS_IMAGE) $(BUILD_ARGS) --target fv3gfs-fms .
	docker build -f $(DOCKERFILE) -t $(ESMF_IMAGE) $(BUILD_ARGS) --target fv3gfs-esmf .
	docker build -f $(DOCKERFILE) -t $(SERIALBOX_IMAGE) $(BUILD_ARGS) --target fv3gfs-environment-serialbox .

push_image_%:
	docker tag $(GCR_URL)/$*:$(DEP_TAG_NAME) $(GCR_URL)/$*:$(DEP_TAG_NAME)-$(COMMIT_SHA)
	docker push $(GCR_URL)/$*:$(DEP_TAG_NAME)
	docker push $(GCR_URL)/$*:$(DEP_TAG_NAME)-$(COMMIT_SHA)

## push container images of dependencies to GCP 
push_deps: push_image_mpi-build push_image_fms-build push_image_esmf-build push_image_serialbox-build

pull_deps: ## pull container images of dependencies from GCP (for faster builds)
	docker pull $(MPI_IMAGE)
	docker pull $(FMS_IMAGE)
	docker pull $(ESMF_IMAGE)
	docker pull $(SERIALBOX_IMAGE)

enter: ## run and enter production container for development
	docker run --rm \
		-v $(shell pwd)/FV3:/FV3 $(OTHER_MOUNTS) \
		-w /FV3 -it $(COMPILED_IMAGE) bash

enter_serialize: ## run and enter serialization container for development
	docker run --rm \
		-v $(shell pwd)/FV3:/FV3/original $(OTHER_MOUNTS) \
		-w /FV3 -it $(SERIALIZE_IMAGE) bash

test: ## run tests (set COMPILED_TAG_NAME to override default)
	pytest tests/pytest --capture=no --verbose --refdir $(shell pwd)/tests/pytest/reference/circleci --image_version $(COMPILED_TAG_NAME)

build_native: ## build FV3 locally (assuming all tools and dependencies are available in the environment)
	$(MAKE) -j 8 -C FV3 GCOV=Y


test_native: DIR=coverage_$(shell date -Is)
test_native: ## run native tests (all tools and build dependencies are assumed to be available in the environment)
	find FV3 -type f -name '*.gcda' -delete
	pytest --native tests/pytest
	pytest FV3/wrapper/tests/
	mkdir -p $(DIR) && \
		cd $(DIR)  && \
		gcovr -d -r ../FV3 --html --html-details -o index.html

test_wrapper:
	$(MAKE) -C FV3/wrapper/ test

docs_wrapper:
	$(MAKE) -C FV3/wrapper/ docs

lint:
	pre-commit run flake8 --all-files

reformat:
	pre-commit run --all-files

clean: ## cleanup source tree and test output
	(cd FV3 && make clean)
	$(RM) -f inputdata
	$(RM) -rf tests/pytest/output/*

setup-hooks:
	pre-commit install
