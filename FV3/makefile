SHELL = /bin/sh

include conf/configure.fv3

FMS_DIR?=
fms_exist=$(wildcard $(FMS_DIR))
FFLAGS   += -I$(FMS_DIR) -Icoarse_graining -Igfsphysics -Iipd -Icpl -Iio -Iatmos_cubed_sphere -Iccpp/driver -I../stochastic_physics $(ESMF_INC)
CPPDEFS  += -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR)

# Flag to CCPP build for 32bit dynamics
ifeq ($(32BIT),Y)
  DYN32 = Y
else
  DYN32 = N
endif

CWD      := $(shell pwd)
SERIALBOX_OUTDIR?=
SERIALBOX_APP?=/serialbox
PPSER_PY?=$(SERIALBOX_APP)/install/python/pp_ser/pp_ser.py
PPSER_FLAGS = --verbose --ignore-identical -m utils_ppser_kbuff

FV3_EXE  = fv3.exe
FV3CAP_LIB  = libfv3cap.a

PWD :=$(shell pwd)
TOP_LEVEL_OBJS_RELATIVE = atmos_model.o module_fv3_config.o
TOP_LEVEL_OBJS_ABSOLUTE = $(addprefix $(PWD)/, $(TOP_LEVEL_OBJS_RELATIVE))

# Linking flags
FV3_LIB_DIRS = atmos_cubed_sphere io ipd gfsphysics  ../stochastic_physics cpl coarse_graining
FV3_LIBS_ABS = $(addprefix -L$(PWD)/, $(FV3_LIB_DIRS))
FV3_LIB_FLAGS = -lfv3core -lfv3io -lipd -lgfsphys -lstochastic_physics -lfv3cpl -lfv3coarse_graining $(FV3_LIBS_ABS)

all: $(FV3_EXE)


install: libs $(FV3_EXE)
	mkdir -p $(PREFIX)/bin $(PREFIX)/lib
	install -t $(PREFIX)/bin/ $(FV3_EXE)


nems: libs fms_check
	$(MAKE) $(FV3CAP_LIB) $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)
	$(MAKE) esmf_make_fragment FMS_DIR=$(FMS_DIR)

fms_check:
ifndef FMS_DIR
	$(error ERROR: FMS_DIR variable is unset)
endif

ifneq (,$(findstring CCPP,$(CPPDEFS)))
libs: fms_check
	$(MAKE) -C coarse_graining       $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)
	$(MAKE) -C cpl                   $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)
	$(MAKE) -C gfsphysics            $(MAKE_OPTS) FMS_DIR=$(FMS_DIR) 32BIT=N  DYN32=$(DYN32) # force gfs physics to 64bit, flag to CCPP build for 32bit dynamics
	$(MAKE) -C ccpp/driver           $(MAKE_OPTS) FMS_DIR=$(FMS_DIR) 32BIT=N  DYN32=$(DYN32) # force gfs physics to 64bit, flag to CCPP build for 32bit dynamics
	$(MAKE) -C ipd                   $(MAKE_OPTS) FMS_DIR=$(FMS_DIR) 32BIT=N  # force gfs physics to 64bit
	$(MAKE) -C io                    $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)
	$(MAKE) -C atmos_cubed_sphere    $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)
	$(MAKE) -C ../stochastic_physics $(MAKE_OPTS) FMS_DIR=$(FMS_DIR) 32BIT=N  # force gfs physics to 64bit

libs_no_dycore: fms_check
	$(MAKE) -C coarse_graining       $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)
	$(MAKE) -C cpl                   $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)
	$(MAKE) -C gfsphysics            $(MAKE_OPTS) FMS_DIR=$(FMS_DIR) 32BIT=N  DYN32=$(DYN32) # force gfs physics to 64bit, flag to CCPP build for 32bit dynamics
	$(MAKE) -C ccpp/driver           $(MAKE_OPTS) FMS_DIR=$(FMS_DIR) 32BIT=N  DYN32=$(DYN32) # force gfs physics to 64bit, flag to CCPP build for 32bit dynamics
	$(MAKE) -C ipd                   $(MAKE_OPTS) FMS_DIR=$(FMS_DIR) 32BIT=N  # force gfs physics to 64bit
	$(MAKE) -C io                    $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)

FV3_LIBS = ccpp/driver/libccppdriver.a atmos_cubed_sphere/libfv3core.a io/libfv3io.a ipd/libipd.a gfsphysics/libgfsphys.a ../stochastic_physics/libstochastic_physics.a cpl/libfv3cpl.a coarse_graining/libfv3coarse_graining.a
$(FV3_EXE): atmos_model.o coupler_main.o module_fv3_config.o
	$(LD) -o $@ $^ $(NCEPLIBS) $(LDFLAGS)

else
libs: fms_check
	$(MAKE) -C coarse_graining       $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)
	$(MAKE) -C cpl                   $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)
	$(MAKE) -C gfsphysics            $(MAKE_OPTS) FMS_DIR=$(FMS_DIR) 32BIT=N  # force gfs physics to 64bit
	$(MAKE) -C ipd                   $(MAKE_OPTS) FMS_DIR=$(FMS_DIR) 32BIT=N  # force gfs physics to 64bit
	$(MAKE) -C io                    $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)
	$(MAKE) -C atmos_cubed_sphere    $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)
	$(MAKE) -C ../stochastic_physics $(MAKE_OPTS) FMS_DIR=$(FMS_DIR) 32BIT=N  # force gfs physics to 64bit

libs_no_dycore: fms_check
	$(MAKE) -C coarse_graining       $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)
	$(MAKE) -C cpl                   $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)
	$(MAKE) -C gfsphysics            $(MAKE_OPTS) FMS_DIR=$(FMS_DIR) 32BIT=N  DYN32=$(DYN32) # force gfs physics to 64bit, flag to CCPP build for 32bit dynamics
	$(MAKE) -C ipd                   $(MAKE_OPTS) FMS_DIR=$(FMS_DIR) 32BIT=N  # force gfs physics to 64bit
	$(MAKE) -C io                    $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)

FV3_LIBS = atmos_cubed_sphere/libfv3core.a io/libfv3io.a ipd/libipd.a gfsphysics/libgfsphys.a ../stochastic_physics/libstochastic_physics.a cpl/libfv3cpl.a coarse_graining/libfv3coarse_graining.a

$(FV3_EXE): atmos_model.o coupler_main.o module_fv3_config.o
	$(LD)  -o $@  $^ $(FV3_LIB_FLAGS) $(NCEPLIBS) $(LDFLAGS)

endif


$(FV3CAP_LIB): atmos_model.o module_fv3_config.o module_fcst_grid_comp.o time_utils.o fv3_cap.o
	ar rv $(FV3CAP_LIB) $?

# add "libs" as dependency to all .o files
MKDEPENDS = ./mkDepends.pl -d libs
DEPEND_FILES = time_utils.F90 module_fv3_config.F90 atmos_model.F90 module_fcst_grid_comp.F90 fv3_cap.F90 coupler_main.F90

# For CCPP, check if SIONlib is used and set linker flags accordingly
ifneq (,$(findstring CCPP,$(CPPDEFS)))
ifneq (,$(findstring SION,$(CPPDEFS)))
  SIONLIB_LINK_FLAGS = $(SIONLIB_LIB)
else
  SIONLIB_LINK_FLAGS =
endif
endif

ifneq (,$(findstring CCPP,$(CPPDEFS)))
esmf_make_fragment:
	@rm -rf nems_dir; mkdir nems_dir
	@cp $(FV3CAP_LIB) ccpp/driver/libccppdriver.a atmos_cubed_sphere/libfv3core.a io/libfv3io.a ipd/libipd.a gfsphysics/libgfsphys.a cpl/libfv3cpl.a ../stochastic_physics/libstochastic_physics.a nems_dir
	@cp fv3gfs_cap_mod.mod nems_dir
	@echo "# ESMF self-describing build dependency makefile fragment" > fv3.mk
	@echo "# src location $(PWD)" >> fv3.mk
	@echo  >> fv3.mk
	@echo "ESMF_DEP_FRONT     = fv3gfs_cap_mod"  >> fv3.mk
	# additional include files needed for PGI
	#@echo "ESMF_DEP_INCPATH   = $(PWD)/nems_dir" >> fv3.mk
	@echo "ESMF_DEP_INCPATH   = $(PWD) $(addprefix $(PWD)/, nems_dir ccpp/driver atmos_cubed_sphere io gfsphysics cpl ipd ../stochastic_physics)" >> fv3.mk
	@echo "ESMF_DEP_CMPL_OBJS ="            >> fv3.mk
	@echo "ESMF_DEP_LINK_OBJS = $(addprefix $(PWD)/nems_dir/, libfv3cap.a libccppdriver.a libfv3core.a libfv3io.a libipd.a libgfsphys.a libfv3cpl.a libstochastic_physics.a) $(SIONLIB_LINK_FLAGS)" >> fv3.mk
	@echo "ESMF_DEP_SHRD_PATH ="            >> fv3.mk
	@echo "ESMF_DEP_SHRD_LIBS ="            >> fv3.mk
	@echo
	@echo "Finished generating ESMF self-describing build dependency makefile fragment:" fv3.mk
	@echo
else
esmf_make_fragment:
	@rm -rf nems_dir; mkdir nems_dir
	@cp $(FV3CAP_LIB) atmos_cubed_sphere/libfv3core.a io/libfv3io.a ipd/libipd.a gfsphysics/libgfsphys.a cpl/libfv3cpl.a ../stochastic_physics/libstochastic_physics.a nems_dir
	@cp fv3gfs_cap_mod.mod nems_dir
	@echo "# ESMF self-describing build dependency makefile fragment" > fv3.mk
	@echo "# src location $(PWD)" >> fv3.mk
	@echo  >> fv3.mk
	@echo "ESMF_DEP_FRONT     = fv3gfs_cap_mod"  >> fv3.mk
	# additional include files needed for PGI
	#@echo "ESMF_DEP_INCPATH   = $(PWD)/nems_dir" >> fv3.mk
	@echo "ESMF_DEP_INCPATH   = $(PWD) $(addprefix $(PWD)/, nems_dir atmos_cubed_sphere io gfsphysics cpl ipd ../stochastic_physics)" >> fv3.mk
	@echo "ESMF_DEP_CMPL_OBJS ="                 >> fv3.mk
	@echo "ESMF_DEP_LINK_OBJS = $(addprefix $(PWD)/nems_dir/, libfv3cap.a libfv3core.a libfv3io.a libipd.a libgfsphys.a libfv3cpl.a libstochastic_physics.a)" >> fv3.mk
	@echo "ESMF_DEP_SHRD_PATH ="                 >> fv3.mk
	@echo "ESMF_DEP_SHRD_LIBS ="                 >> fv3.mk
	@echo
	@echo "Finished generating ESMF self-describing build dependency makefile fragment:" fv3.mk
	@echo
endif

# fv3 library installation defaults (for NEMS):
DESTDIR  := $(PWD)
INSTDIR  := FV3_INSTALL

nemsinstall: nems
	@mkdir -p $(DESTDIR)/$(INSTDIR)
	@cp nems_dir/* $(DESTDIR)/$(INSTDIR)
	@sed -e 's;'$(PWD)/nems_dir';'$(DESTDIR)/$(INSTDIR)';g' fv3.mk > $(DESTDIR)/$(INSTDIR)/fv3.mk
	@echo Installation into \"$(DESTDIR)/$(INSTDIR)\" complete!
	@echo

serialize: serialize_outdir_check
	@make serialize_preprocess
	$(MAKE) -C $(SERIALBOX_OUTDIR) clean
	$(MAKE) -C $(SERIALBOX_OUTDIR) build_serializer

serialize_outdir_check:
ifeq ($(strip $(SERIALBOX_OUTDIR)),)
	$(error ERROR: SERIALBOX_OUTDIR is unset)
endif

serialize_preprocess: serialize_outdir_check
	@echo "preprocessing for serialization"
	@mkdir -p $(SERIALBOX_OUTDIR)/atmos_cubed_sphere/model
	@mkdir -p $(SERIALBOX_OUTDIR)/atmos_cubed_sphere/driver/fvGFS
	@mkdir -p $(SERIALBOX_OUTDIR)/gfsphysics/GFS_layer
	@mkdir -p $(SERIALBOX_OUTDIR)/atmos_cubed_sphere/tools
	@make pp_ser_calls
	@cp -r -u  $(CWD)/* $(SERIALBOX_OUTDIR)


pp_ser_calls: serialize_outdir_check
	@echo "preprocessing for serialization, just the pp_ser.py calls"
	@python3 $(PPSER_PY) $(PPSER_FLAGS)  --output-dir=$(SERIALBOX_OUTDIR)/ $(CWD)/*.F90
	@python3 $(PPSER_PY) $(PPSER_FLAGS)  --output-dir=$(SERIALBOX_OUTDIR)/atmos_cubed_sphere/model $(CWD)/atmos_cubed_sphere/model/*.F90
	@python3 $(PPSER_PY) $(PPSER_FLAGS)  --output-dir=$(SERIALBOX_OUTDIR)/atmos_cubed_sphere/driver/fvGFS $(CWD)/atmos_cubed_sphere/driver/fvGFS/*.F90
	@python3 $(PPSER_PY) $(PPSER_FLAGS)  --output-dir=$(SERIALBOX_OUTDIR)/gfsphysics/GFS_layer $(CWD)/gfsphysics/GFS_layer/*.F90
	@python3 $(PPSER_PY) $(PPSER_FLAGS)  --output-dir=$(SERIALBOX_OUTDIR)/atmos_cubed_sphere/tools $(CWD)/atmos_cubed_sphere/tools/fv_grid_tools.F90
	@python3 $(PPSER_PY) $(PPSER_FLAGS)  --output-dir=$(SERIALBOX_OUTDIR)/atmos_cubed_sphere/tools $(CWD)/atmos_cubed_sphere/tools/fv_restart.F90
	@python3 $(PPSER_PY) $(PPSER_FLAGS)  --output-dir=$(SERIALBOX_OUTDIR)/atmos_cubed_sphere/tools $(CWD)/atmos_cubed_sphere/tools/test_cases.F90

.PHONY: clean cleanall serialize serialize_preprocess fms_check serialize_outdir_check libs

clean: wrapper_clean
	@echo "Cleaning ... "
	@echo
	(cd gfsphysics            && make clean)
	(cd ccpp/driver           && make clean)
	(cd ipd                   && make clean)
	(cd ../stochastic_physics && make clean)
	(cd io                    && make clean)
	(cd atmos_cubed_sphere    && make clean)
	(cd cpl                   && make clean)
	(cd coarse_graining       && make clean)
	$(RM) -f $(FV3_EXE) $(FV3CAP_LIB) *.o *.mod *.i90 *.lst depend

clean_no_dycore:
	@echo "Cleaning ... "
	@echo
	(cd gfsphysics            && make clean)
	(cd ccpp/driver           && make clean)
	(cd ipd                   && make clean)
	(cd ../stochastic_physics && make clean)
	(cd io                    && make clean)
	(cd cpl                   && make clean)
	(cd coarse_graining       && make clean)
	$(RM) -f $(FV3_EXE) $(FV3CAP_LIB) *.o *.mod *.i90 *.lst depend

cleanall: clean
	$(RM) -rf nems_dir fv3.mk $(INSTDIR)
	$(RM) -f conf/modules.fv3
	$(RM) -f conf/configure.fv3
	$(RM) -rf $(SERIALBOX_OUTDIR)


# do not include 'depend' file if the target contains string 'clean'
ifneq (clean,$(findstring clean,$(MAKECMDGOALS)))
    -include depend
endif

# Wrapper related build rules
TEMPLATES_RELATIVE =  dynamics_data.F90 flagstruct_data.F90 physics_data.F90 _wrapper.pyx
TEMPLATES_IN = $(addprefix wrapper/templates/,$(TEMPLATES_RELATIVE))
TEMPLATES_OUT = $(addprefix wrapper/lib/,$(TEMPLATES_RELATIVE))
WRAPPER_OBJECTS=$(TEMPLATES_OUT:.F90=.o) wrapper/lib/coupler_lib.o
PROPERTIES_FILES = wrapper/fv3gfs/wrapper/dynamics_properties.json \
	wrapper/fv3gfs/wrapper/flagstruct_properties.json \
	wrapper/fv3gfs/wrapper/physics_properties.json
SETUP_PY_LIBS = $(TOP_LEVEL_OBJS_ABSOLUTE) \
			  -L$(PWD) \
			  $(FV3_LIB_FLAGS) \
			  $(NCEPLIBS) \
			  $(LDFLAGS) \
			  $(MPI_LDFLAGS) \
			  -lmvec \
			  -lm

wrapper_clean:
	cd wrapper/lib && rm -f *.o *.mod *.lst *.c depend $(TEMPLATES_RELATIVE)

wrapper_build:$(WRAPPER_OBJECTS)
	cd wrapper && SETUP_PY_LIBS="$(SETUP_PY_LIBS)" python3 setup.py build_ext --inplace
	cd wrapper && SETUP_PY_LIBS="$(SETUP_PY_LIBS)" python3 setup.py bdist_wheel

$(TEMPLATES_OUT): $(TEMPLATES_IN) $(PROPERTIES_FILES)
	# fill_templates.py needs to be run in the same directory as the outputs
	cd wrapper/lib && python3 ../fill_templates.py $(TEMPLATES_RELATIVE)

# Specify dependency structure of the wrapper object files. Would be easier to
# include in the DEPEND_FILES macro above so that mkDepends.pl could generate
# the dependency graph. This would however, setup a chicken or egg problem where
# wrapper/fill_templates.py would need to be run before generating running
# mkDepends.pl. And we don't want to do that for non-wrapper builds.
wrapper/lib/physics_data.o: wrapper/lib/physics_data.F90 wrapper/lib/dynamics_data.o  atmos_model.o
wrapper/lib/coupler_lib.o: wrapper/lib/coupler_lib.F90 wrapper/lib/dynamics_data.o atmos_model.o
wrapper/lib/flagstruct_data.o: wrapper/lib/flagstruct_data.F90 atmos_model.o
wrapper/lib/dynamics_data.o: wrapper/lib/dynamics_data.F90 atmos_model.o

include conf/make.rules
