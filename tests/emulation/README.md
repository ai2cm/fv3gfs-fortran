emulation
=========

A basic implementation of `emulation` is provided to test the call_py_fort calls from fv3gfs.  Downstream projects should provide their own definitions `emulation` package by defining two functions the two functions `store`  and `microphysics`.

The interaction points with python are located in `GFS_physics_driver.f90` as the `set_state`, `get_state`, and `call_function` commands.   Using `make build_emulation` from the parent directory will provide a compiled image with call_py_fort functionality enabled.  Two namelist flags can be used to turn off storage (`gfs_physics_nml.save_zc_microphysics`) and emulation (`gfs_physics_nml.emulate_zc_microphysics`) while call_py_fort is enabled.

The emulation should overwrite the fields `air_temperature_output`, `specific_humidity_output`, `cloud_water_mixing_ratio_output`, `total_precipitation`, to control the microphysical updates during runtime. 

Available input state fields for emulation
------------------------------------------
- model_time
- latitude
- longitude
- pressure_thickness_of_atmospheric_layer
- air_pressure
- surface_air_pressure
- air_temperature_input
- specific_humidity_input
- cloud_water_mixing_ratio_input
- air_temperature_two_time_steps_back
- specific_humidity_two_time_steps_back
- surface_air_pressure_two_time_steps_back
- air_temperature_at_previous_time_step
- specific_humidity_at_previous_time_step
- surface_air_pressure_at_previous_time_step
- ratio_of_snowfall_to_rainfall
- tendency_of_rain_water_mixing_ratio_due_to_microphysics

Notes
-----

The usage of call_py_fort results in some subtle differences with python initialization that can lead to some hard-to-debug issues.

- the current directory is not added to `PYTHONPATH`
- `sys.argv` is not initialized and will lead to errors for anything that expects to use it (this was an issue for tensorflow).  Remedied by instantiating in the module prior to imports that need it.  E.g.,

```
import sys
if not hasattr(sys, "argv"):
    sys.argv = [""]

import tensorflow
```
