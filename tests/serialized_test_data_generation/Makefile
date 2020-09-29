GCR_URL = us.gcr.io/vcm-ml
STORAGE_BUCKET=gs://vcm-fv3gfs-serialized-regression-data
#<some large conceptual version change>.<serialization statement change>.<hotfix>
FORTRAN_VERSION=7.1.1
CWD=$(shell pwd)
EXPERIMENT ?=c12_6ranks_standard

TEST_DATA_HOST ?=$(CWD)/test_data/$(EXPERIMENT)
RUNDIR_HOST ?=$(CWD)/rundir/$(EXPERIMENT)

FORTRAN_DIR=$(CWD)/../../
COMPILED_IMAGE ?= $(GCR_URL)/fv3gfs-compiled:$(FORTRAN_VERSION)-serialize

RUNDIR_CONTAINER=/rundir
TEST_DATA_CONTAINER=$(RUNDIR_CONTAINER)/test_data

TEST_DATA_TARFILE=dat_files.tar.gz
TEST_DATA_BUCKET=$(STORAGE_BUCKET)/$(FORTRAN_VERSION)/$(EXPERIMENT)
FV3CONFIG_DIR=$(CWD)/external/fv3config
LOOKUP_BUCKET=$(shell eval gsutil -q stat $(TEST_DATA_BUCKET)/MetaData-Generator_rank0.json || echo 1)

install_deps:
	pip3 install -r ../../requirements.txt	

setup_rundir: 
	if [ ! -d $(RUNDIR_HOST) ]; then \
	write_run_directory configs/$(EXPERIMENT).yml $(RUNDIR_HOST) && \
	cp submit_job.sh $(RUNDIR_HOST) && cp count_ranks.py $(RUNDIR_HOST) && \
	git rev-parse HEAD > $(RUNDIR_HOST)/fortran_sha.txt ; fi

compile_fortran:
	cd $(FORTRAN_DIR) && DOCKER_BUILDKIT=1 SERIALIZE_IMAGE=$(COMPILED_IMAGE) $(MAKE) build_serialize_gt4py_dev

run_model_make_data:
	docker run --network host --rm -v $(RUNDIR_HOST):$(RUNDIR_CONTAINER) -v $(TEST_DATA_HOST):$(TEST_DATA_CONTAINER) -it $(COMPILED_IMAGE)-gt4pydev /rundir/submit_job.sh

generate_test_data: setup_rundir compile_fortran run_model_make_data

pack_test_data:
	if [ ! -d $(TEST_DATA_HOST)/$(TEST_DATA_TARFILE) ]; then \
	cd $(TEST_DATA_HOST) && tar  --exclude=input.nml --exclude=*.json  --exclude=$(TEST_DATA_TARFILE) \
	-czf $(TEST_DATA_TARFILE) . ;fi 

	for number in 0 1 2 3 4 5 6 7 8 9 ; do \
		cd $(TEST_DATA_HOST) && rm -f Gen*rank$$number*.dat ; \
	done
unpack_test_data:
	if [ -f $(TEST_DATA_TARPATH) ]; then \
	cd $(TEST_DATA_HOST) && tar -xf $(TEST_DATA_TARFILE) && \
	rm $(TEST_DATA_TARFILE); fi

push_test_data:
	echo $(LOOKUP_BUCKET)
	gsutil -m rsync $(TEST_DATA_HOST) $(TEST_DATA_BUCKET)	

generate_and_push_data: setup_rundir run_model_make_data pack_test_data push_test_data

generate_coverage: install_deps
	rm -rf coverage
	EXPERIMENT=fv3config_coverage $(MAKE) setup_rundir 	
	cd $(FORTRAN_DIR) && DOCKER_BUILDKIT=1 $(MAKE) build_coverage
	mkdir coverage
	docker run -it --rm --network host -v $(PWD)/coverage:/coverage -v $(CWD)/rundir/fv3config_coverage:$(RUNDIR_CONTAINER) $(GCR_URL)/fv3gfs-compiled:gcov bash -c "pip install gcovr; cd /coverage; mkdir physics; cd physics; gcovr -d -r /FV3/gfsphysics --html --html-details -o index.html; cd ../; mkdir dycore; cd dycore; gcovr -d -r /FV3/atmos_cubed_sphere --html --html-details -o index.html"
	@echo "==== Coverage ananlysis done. Now open coverage/dycore/index.html coverage/physics/index.html in your browser ===="



.PHONY: install_deps setup_rundir compile_fortran run_model_make_data generate_test_data pack_test_data \
	unpack_test_data push_test_data generate_and_push_data generate_coverage
