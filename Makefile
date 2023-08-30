# setup (use XXX=<value> make <target> to override)
.PHONY: help build build_repro build_debug build_wrapper clean

help:
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

build: ## build repro and debug mode versions FV3 locally (assuming all tools and dependencies are available in the environment)
	$(MAKE) build_repro
	$(MAKE) clean_fortran_build_artifacts
	$(MAKE) build_debug

build_repro: ## build repro mode version of FV3 locally (assuming all tools and dependencies are available in the environment)
	$(MAKE) -j 8 -C FV3
	mkdir -p bin
	cp FV3/fv3.exe bin/fv3.repro.exe

build_debug: ## build debug mode version of FV3 locally in debug mode
    # Note DEBUG and REPRO cannot be set to Y simultaneously; CALLPYFORT can
    # also not currently be set in debug mode without failures (see GitHub issue
    # #365).
	CALLPYFORT= DEBUG=Y REPRO= $(MAKE) -j 8 -C FV3
	mkdir -p bin
	cp FV3/fv3.exe bin/fv3.debug.exe

build_wrapper:
	$(MAKE) -j 8 -C FV3 wrapper_build

test_fortran:
	pytest tests/pytest -v

test_fortran_basic:
	pytest tests/pytest -v -m 'basic'

test_fortran_coarse_graining:
	pytest tests/pytest -v -m 'coarse'

test_fortran_emulation:
	pytest tests/pytest -v -m 'emulation'

test_fortran_unmarked:
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
