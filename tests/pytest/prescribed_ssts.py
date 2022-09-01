"""Code to help run the prescribed SST regression test.

Writing a prescribed SST file
-----------------------------

In general the data_override functionality of FMS is very powerful.  Given a
time-varying SST pattern on a latitude-longitude grid, it is able to interpolate
values onto the model grid at the appropriate time using the interpolation
method specified in the data_table.  There are a few gotchas, however:

- FMS prefers the order of the dimensions of the SST variable be time, latitude,
  longitude.

- In the SST file, the time, latitude, and longitude must include the
  appropriate "axis" attributes; this is how FMS determines which axes
  correspond to the time, latitude and longitude dimensions without relying on
  hard-coded names.

- When writing a dataset using xarray, we must override the _FillValue encoding
  attribute of the SST variable.  Otherwise the horizontal interpolation routine
  of FMS will return nonsense. 

- When writing the dataset using xarray, we must also ensure that the time
  variable is encoded as a float.  FMS will raise an error if it is encoded as
  an integer (which is xarray's default choice).

- Finally, when writing the dataset, the time dimension must be encoded as a
  "record" dimension (also known as an "unlimited" dimension).

Running with prescribed SSTs
----------------------------

Once a prescribed SST file has been created, one can configure a simulation to
use those SSTs by adding a data_table to the model that looks like the
following:

"ATM", "sea_surface_temperature", "sea_surface_temperature", "INPUT/sst.nc", "bilinear", 1.0

and setting the gfs_physics_nml.use_prescribed_sea_surface_properties namelist
parameter to True. 

In addition, the data_override functionality in FMS requires
that grid files be provided to model.  These can be generated using the
make_hgrid tool in the FRE_NCTOOLS library and must have twice the resolution of
the target model. Note that running with a grid specified in this manner
produces results that are NOT bit-for-bit identical with simulations that are
run without a prescribed grid due to roundoff error (order 1e-14 degrees).

Description of the regression test
----------------------------------

The regression test involves prescribing a latitudinally dependent SST pattern
which is increased uniformly by 2 K every 30 minutes.  In between those 30
minute intervals, the SST must be interpolated in time.  In the context of the
regression test, we therefore expect the SST to increase uniformly by 1 K at
each of the four 15-minute diagnostics intervals.  Using an SST with an analytic
pattern makes it straightforward to verify that the interpolation in both space
and time is working as expected.

The analytic SST pattern is derived from Equation (1) of the Appendix of the
following paper:

Neale, R. B., & Hoskins, B. J. (2000). A standard test for AGCMs including their
physical parametrizations: I: the proposal. Atmospheric Science Letters, 1(2),
101–107. https://doi.org/10.1006/asle.2000.0022
"""
import copy
import os

import fv3config
import numpy as np
import xarray as xr


GRID_DATA_ROOT = "gs://vcm-fv3config/data/grid_data/v1.0"
FILL_VALUE = -99999.0


def aquaplanet_sst_pattern(lat):
    sst = 273 + 27 * (1 - np.sin(3 * np.deg2rad(lat) / 2) ** 2)
    return xr.where(np.abs(lat) < 60, sst, 273).rename("sea_surface_temperature")


def assign_encoding(da, **kwargs):
    da = da.copy(deep=False)
    da.encoding.update(kwargs)
    return da


def create_sst_dataset(tmpdir):
    lat = np.arange(-89.5, 90, 1)
    lon = np.arange(0.5, 360, 1)
    times = xr.cftime_range("2016-08-01", periods=4, freq="30T", calendar="julian")

    time = xr.DataArray(times, coords=[times], dims=["time"], name="time")
    lon = xr.DataArray(lon, coords=[lon], dims=["lon"], name="lon")
    lat = xr.DataArray(lat, coords=[lat], dims=["lat"], name="lat")

    base_sst = aquaplanet_sst_pattern(lat)
    base_sst, _, = xr.broadcast(base_sst, lon)
    sst = xr.concat([base_sst, base_sst + 2, base_sst + 4, base_sst + 6], dim=time)

    # Order dimensions in the manner that FMS prefers.
    sst = sst.transpose("time", "lat", "lon")

    # Assign required encoding attributes for FMS.
    sst = assign_encoding(sst, _FillValue=FILL_VALUE)
    sst["lat"] = sst.lat.assign_attrs(axis="Y")
    sst["lon"] = sst.lon.assign_attrs(axis="X")
    sst["time"] = assign_encoding(sst.time, dtype=float).assign_attrs(axis="T")

    # Write out dataset to a file, ensuring "time" is a record dimension.
    ds = sst.to_dataset()
    path = os.path.join(str(tmpdir), "sst.nc")
    ds.to_netcdf(path, unlimited_dims=["time"])


def compute_expected_ssts(lat):
    base_sst = aquaplanet_sst_pattern(lat)
    return xr.concat([base_sst, base_sst + 1, base_sst + 2, base_sst + 3], dim="time")


def validate_ssts(ds):
    ocean = ds.SLMSKsfc == 0
    result = ds.TMPsfc.where(ocean)
    expected = compute_expected_ssts(ds.lat).transpose(*result.dims).where(ocean)
    xr.testing.assert_allclose(result, expected, atol=0.01)


def grid_file_assets(resolution):
    files = []
    root = os.path.join(GRID_DATA_ROOT, resolution)
    for tile in range(1, 7):
        filename = f"{resolution}_grid.tile{tile}.nc"
        asset = fv3config.get_asset_dict(root, filename, "INPUT/", filename)
        files.append(asset)

    filename = f"{resolution}_mosaic.nc"
    mosaic = fv3config.get_asset_dict(root, filename, "INPUT/", "grid_spec.nc")
    files.append(mosaic)
    return files


def data_table_asset():
    data_table = b'"ATM", "sea_surface_temperature", "sea_surface_temperature", "INPUT/sst.nc", "bilinear", 1.0'
    return fv3config.get_bytes_asset_dict(data_table, ".", "data_table")


def sst_file_asset(root):
    return fv3config.get_asset_dict(root, "sst.nc", "INPUT/", "sst.nc")


def get_patch_files(tmpdir):
    grid_files = grid_file_assets("C12")
    sst_file = sst_file_asset(str(tmpdir))
    data_table = data_table_asset()
    return grid_files + [sst_file, data_table]
