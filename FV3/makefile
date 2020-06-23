SHELL = /bin/sh

include conf/configure.fv3

FMS_DIR?=
fms_exist=$(wildcard $(FMS_DIR))

FFLAGS   += -I$(FMS_DIR) -Icoarse_graining -Igfsphysics -Iipd -Icpl -Iio -Iatmos_cubed_sphere -Iccpp/driver -I../stochastic_physics
CPPDEFS  += -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR)

# Flag to CCPP build for 32bit dynamics
ifeq ($(32BIT),Y)
  DYN32 = Y
else
  DYN32 = N
endif

CWD      := $(shell pwd)
SERIALBOX_OUTDIR?=
SERIALBOX_DIR?=/serialbox2
PPSER_PY?=$(SERIALBOX_DIR)/install/python/pp_ser/pp_ser.py
PPSER_FLAGS = --verbose --ignore-identical -m utils_ppser_kbuff

FV3_EXE  = fv3.exe
FV3CAP_LIB  = libfv3cap.a

all: libs fms_check
	$(MAKE) $(FV3_EXE) $(MAKE_OPTS) FMS_DIR=$(FMS_DIR)

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

$(FV3_EXE): atmos_model.o coupler_main.o module_fv3_config.o ccpp/driver/libccppdriver.a atmos_cubed_sphere/libfv3core.a io/libfv3io.a ipd/libipd.a gfsphysics/libgfsphys.a ../stochastic_physics/libstochastic_physics.a cpl/libfv3cpl.a
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

$(FV3_EXE): atmos_model.o coupler_main.o module_fv3_config.o atmos_cubed_sphere/libfv3core.a io/libfv3io.a ipd/libipd.a gfsphysics/libgfsphys.a ../stochastic_physics/libstochastic_physics.a cpl/libfv3cpl.a coarse_graining/libfv3coarse_graining.a
	$(LD) -o $@ $^ $(NCEPLIBS) $(LDFLAGS)
endif

$(FV3CAP_LIB): atmos_model.o module_fv3_config.o module_fcst_grid_comp.o time_utils.o fv3_cap.o
	ar rv $(FV3CAP_LIB) $?

atmos_model.o : atmos_model.F90 module_fv3_config.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) $(ESMF_INC) -c atmos_model.F90

module_fv3_config.o: module_fv3_config.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) $(ESMF_INC) -c module_fv3_config.F90
module_fcst_grid_comp.o: module_fcst_grid_comp.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) $(ESMF_INC) -c module_fcst_grid_comp.F90
time_utils.o: time_utils.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) $(ESMF_INC) -c time_utils.F90
fv3_cap.o: fv3_cap.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) $(ESMF_INC) -c fv3_cap.F90

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
	SERIALBOX_DIR=$(SERIALBOX_DIR) PPSER_PY=$(PPSER_PY) bash ./serialize_preprocess.sh $(PWD) $(SERIALBOX_OUTDIR)

.PHONY: clean cleanall clean_no_dycore serialize serialize_preprocess fms_check serialize_outdir_check
clean:
	@echo "Cleaning ... "
	@echo
	(cd gfsphysics            && make clean)
	(cd ccpp/driver           && make clean)
	(cd ipd                   && make clean)
	(cd ../stochastic_physics && make clean)
	(cd io                    && make clean)
	(cd atmos_cubed_sphere    && make clean)
	(cd cpl                   && make clean)
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
	$(RM) -f $(FV3_EXE) $(FV3CAP_LIB) *.o *.mod *.i90 *.lst depend

cleanall: clean
	$(RM) -rf nems_dir fv3.mk $(INSTDIR)
	$(RM) -f conf/modules.fv3
	$(RM) -f conf/configure.fv3
	$(RM) -rf $(SERIALBOX_OUTDIR)

MKDEPENDS = ./mkDepends.pl
include conf/make.rules

# do not include 'depend' file if the target contains string 'clean'
ifneq (clean,$(findstring clean,$(MAKECMDGOALS)))
    -include depend
endif

