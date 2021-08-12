emulation
=========

This is a stripped down set of modules for adding into a prognostic run using `call_py_fort`.  It's currently used to create training datasets directly from data from the microphysics parameterization.

The interaction points with python are located in `GFS_physics_driver.f90` as the `set_state` or `get_state` commands.  These are enabled for the emulation image (compiled using `make build_emulation`) when `-DENABLE_CALLPYFORT` is passed to the compiler.

### Example snippet

```
#ifdef ENABLE_CALLPYFORT
            do k=1,levs
              do i=1,im
                qv_post_precpd(i,k) = Stateout%gq0(i,k,1)
                qc_post_precpd(i,k) = Stateout%gq0(i,k,ntcw)
              enddo
            enddo

            call set_state("air_temperature_output", Stateout%gt0)
            call set_state("specific_humidity_output", qv_post_precpd)
            call set_state("cloud_water_mixing_ratio_output", qc_post_precpd)
```

## Training Data
By default, the training data is saved out to the current working directory with a zarr monitor to state_output.zarr (time, tile, x, y), or individual netCDF files for each time and tile under $(cwd)/netcdf_output.

To change the frequency for which data are saved (defaults to 5 hours [18,000 s]), prescribe the `OUTPUT_FREQ_SEC` environment variable in the runtime image.
