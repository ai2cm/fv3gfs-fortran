# setup (use XXX=<value> make <target> to override)
.PHONY: help clean

help:
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

build_native: ## build FV3 locally (assuming all tools and dependencies are available in the environment)
	$(MAKE) -j 8 -C FV3

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
	pytest --native tests/pytest -v

test_native_fortran_basic:
	pytest --native tests/pytest -v -m 'basic'

test_native_fortran_coarse_graining:
	pytest --native tests/pytest -v -m 'coarse'

test_native_fortran_emulation:
	pytest --native tests/pytest -v -m 'emulation'

test_native_fortran_unmarked:
	pytest --native tests/pytest -v -m 'not basic and not coarse and not emulation'

test_wrapper:
	$(MAKE) -C FV3/wrapper/ test

docs_wrapper:
	$(MAKE) -C FV3/wrapper/ docs

lint:
	pre-commit run flake8 --all-files

reformat:
	pre-commit run --all-files

clean: ## cleanup source tree
	(cd FV3 && make clean)

setup-hooks:
	pre-commit install
