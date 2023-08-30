# setup (use XXX=<value> make <target> to override)
.PHONY: help build build_repro build_debug build_wrapper clean

help:
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

build_native: ## build FV3 locally (assuming all tools and dependencies are available in the environment)
	$(MAKE) build_repro

build_repro: ## build FV3 locally (assuming all tools and dependencies are available in the environment)
	$(MAKE) -j 8 -C FV3
	mkdir -p bin
	cp FV3/fv3.exe bin/fv3.repro.exe

build_debug: ## build FV3 locally in debug mode (DEBUG and REPRO cannot be set to Y simultaneously)
	CALLPYFORT= DEBUG=Y REPRO= $(MAKE) -j 8 -C FV3
	mkdir -p bin
	cp FV3/fv3.exe bin/fv3.debug.exe

build_wrapper:
	$(MAKE) -j 8 -C FV3 wrapper_build

test_native: DIR=coverage_$(shell date -Is)
test_native: ## run native tests (all tools and build dependencies are assumed to be available in the environment)
	find FV3 -type f -name '*.gcda' -delete
	pytest --native tests/pytest
	pytest -v FV3/wrapper/tests/pytest
	pytest -v FV3/wrapper/tests/test_all_mpi_requiring.py
	mkdir -p $(DIR) && \
		cd $(DIR)  && \
		gcovr -d -r ../FV3 --html --html-details -o index.html

test_native_fortran:
	pytest tests/pytest -v

test_native_fortran_basic:
	pytest tests/pytest -v -m 'basic'

test_native_fortran_coarse_graining:
	pytest tests/pytest -v -m 'coarse'

test_native_fortran_emulation:
	pytest tests/pytest -v -m 'emulation'

test_native_fortran_unmarked:
	pytest tests/pytest -v -m 'not basic and not coarse and not emulation'

test_wrapper:
	$(MAKE) -C FV3/wrapper/ test

docs_wrapper:
	$(MAKE) -C FV3/wrapper/ docs

lint:
	pre-commit run flake8 --all-files

reformat:
	pre-commit run --all-files

clean_fortran_build_artifacts:
	$(MAKE) -C FV3 clean

clean_executables:
	$(RM) -rf bin

clean: ## cleanup source tree and executables
	$(MAKE) clean_fortran_build_artifacts
	$(MAKE) clean_executables

setup-hooks:
	pre-commit install
