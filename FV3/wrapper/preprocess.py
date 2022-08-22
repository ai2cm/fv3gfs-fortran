import os
import xarray as xr
import warnings
#import vcm
import numpy as np

# RadInit flags

to_validate = [ 'clear_sky_downward_longwave_flux_at_surface',
                'clear_sky_downward_shortwave_flux_at_surface',
                'clear_sky_upward_longwave_flux_at_surface',
                'clear_sky_upward_longwave_flux_at_top_of_atmosphere',
                'clear_sky_upward_shortwave_flux_at_surface',
                'clear_sky_upward_shortwave_flux_at_top_of_atmosphere',
                'total_sky_downward_longwave_flux_at_surface',
                'total_sky_downward_shortwave_flux_at_top_of_atmosphere',
                'total_sky_upward_longwave_flux_at_surface',
                'total_sky_upward_longwave_flux_at_top_of_atmosphere',
                'total_sky_upward_shortwave_flux_at_surface',
                'total_sky_upward_shortwave_flux_at_top_of_atmosphere']

this_variables = ['pressure_thickness_of_atmospheric_layer',
                'atmosphere_hybrid_a_coordinate',
                'atmosphere_hybrid_b_coordinate',
                'interface_pressure',
                'air_temperature',
                'specific_humidity',
                'cloud_water_mixing_ratio',
                'rain_mixing_ratio',
                'cloud_ice_mixing_ratio',
                'snow_mixing_ratio',
                'graupel_mixing_ratio',
                'cloud_amount',
                'longitude',
                'latitude',
                'orographic_variables',
                'surface_temperature',
                'land_sea_mask',
                'snow_depth_water_equivalent',
                'snow_cover_in_fraction',
                'maximum_snow_albedo_in_fraction',
                'surface_roughness',
                'mean_visible_albedo_with_strong_cosz_dependency',
                'mean_near_infrared_albedo_with_strong_cosz_dependency',
                'mean_visible_albedo_with_weak_cosz_dependency',
                'mean_near_infrared_albedo_with_weak_cosz_dependency',
                'fractional_coverage_with_strong_cosz_dependency',
                'fractional_coverage_with_weak_cosz_dependency',
                'ice_fraction_over_open_water',
                'surface_temperature_over_ice_fraction',
                'ozone_mixing_ratio'] 

def compare_data(data, ref_data, explicit=True, blocking=True):
    """test whether stencil output matches fortran output

    Args:
        data (dict): dictionary of variable names and stencil output
        ref_data (dict): dictionary of variable names and fortran output
        explicit (bool, optional): Flag to print result. Defaults to True.
        blocking (bool, optional): Flag to make failure block progress.
        Defaults to True.
    """

    wrong = []
    flag = True

    for var in data:

        if not np.allclose(
            data[var], ref_data[var], rtol=1e-11, atol=1.0e-13, equal_nan=True
        ):

            wrong.append(var)
            flag = False

        else:

            if explicit:
                print(f"Successfully validated {var}!")

    if blocking:
        assert flag, f"Output data does not match reference data for field {wrong}!"
    else:
        if not flag:
            print(f"Output data does not match reference data for field {wrong}!")


def pressure_at_midpoint_log(delp, toa_pressure = 300, dim = 'z'):
    pi = pressure_at_interface(delp)
    dlogp = np.log(pi).diff(dim)
    output = delp / dlogp
    # ensure that output chunks are the same
    # pressure at interface adds a single chunk at the model surface
    if delp.chunks:
        original_chunk_size = delp.chunks[delp.get_axis_num(dim)]
        output = output.chunk({dim: original_chunk_size})

    return output


def pressure_at_midpoint(pi,dim_outer='z'):
    pi_mid = (pi.isel({dim_outer: slice(0, -1)}) + pi.isel({dim_outer: slice(1, None)})) / 2
    return pi_mid

def pressure_at_interface(delp,dim_center='z', toa_pressure=300):
    top = xr.full_like(delp.isel({dim_center: [0]}), toa_pressure)#.variable
    delp_with_top = xr.concat([top, delp], dim=dim_center)
    pressure = delp_with_top.cumsum(dim_center)
    return pressure

def stack_ds(ds, columns_dict = ["y","x"]):
    ds_out = {}
    for var in ds: 
        ds_out[var] = ds[var].stack(ncolumns=columns_dict).values
    return ds_out

def get_radiation_inputs(ds, columns_dict = ["y","x"]):

    ## Getting State inputs 
    ## Make sure ivflip (vertical index direction control flag)  == 1, and flip z dim 
    ## from surface to toa  
    delp = ds['pressure_thickness_of_atmospheric_layer']
    pi  = pressure_at_interface(delp)
    pm  = pressure_at_midpoint_log(delp)

    ds_columns = ds.stack(ncolumns=columns_dict)
    nz = ds.z.size
    
    p_interface = pi.stack(ncolumns=columns_dict)#ds['interface_pressure']
    p_midpoint  = pm.stack(ncolumns=columns_dict)

    cp = 1004
    Rd = 287.05
    p_ref = 1.0e5
    exner_pressure = (p_midpoint/p_ref)**(Rd/cp)
    temperature =  ds_columns['air_temperature']

    ncolumns = len(ds_columns['ncolumns'])
    
    tracer_names = [
        'specific_humidity', 'cloud_water_mixing_ratio','rain_mixing_ratio',  'cloud_ice_mixing_ratio',
        'snow_mixing_ratio', 'graupel_mixing_ratio', 
        'ozone_mixing_ratio', 'cloud_amount']

    tracer_arrays = np.zeros((ncolumns, nz ,len(tracer_names)))
    for n, tracer_name in enumerate(tracer_names):
        tracer_arrays[:, :, n] = ds_columns[tracer_name].values.swapaxes(1,0)[:,::-1]

    Statein = {}
    Statein['prsi']  = p_interface.values.swapaxes(1,0)[:,::-1]
    Statein['prsl']  = p_midpoint.values.swapaxes(1,0)[:,::-1]
    Statein['tgrs']  = temperature.values.swapaxes(1,0)[:,::-1]
    Statein['prslk'] = exner_pressure.values.swapaxes(1,0)[:,::-1]
    Statein['qgrs']  = tracer_arrays

    ## Getting Grids 
    Grid = {}
    Grid['xlon'] = ds_columns['longitude'].values
    Grid['xlat'] = ds_columns['latitude'].values
    Grid['sinlat'] =  np.sin(Grid['xlat'])
    Grid['coslat'] =  np.cos(Grid['xlat'])

    ## Getting Sfcprop 
    Sfcprop  ={}

    Sfcprop['tsfc']   = ds_columns['surface_temperature'].values
    Sfcprop['slmsk']  = ds_columns['land_sea_mask'].values
    Sfcprop['snowd']  = ds_columns['snow_depth_water_equivalent'].values
    Sfcprop['sncovr'] = ds_columns['snow_cover_in_fraction'].values
    Sfcprop['snoalb'] = ds_columns['maximum_snow_albedo_in_fraction'].values
    Sfcprop['zorl']   = ds_columns['surface_roughness'].values
    Sfcprop['hprime'] = ds_columns['orographic_variables'].isel(orographic_variable=0).values
    Sfcprop['alvsf']  = ds_columns['mean_visible_albedo_with_strong_cosz_dependency'].values
    Sfcprop['alnsf']  = ds_columns['mean_near_infrared_albedo_with_strong_cosz_dependency'].values
    Sfcprop['alvwf']  = ds_columns['mean_visible_albedo_with_weak_cosz_dependency'].values
    Sfcprop['alnwf']  = ds_columns['mean_near_infrared_albedo_with_weak_cosz_dependency'].values
    Sfcprop['facsf']  = ds_columns['fractional_coverage_with_strong_cosz_dependency'].values
    Sfcprop['facwf']  = ds_columns['fractional_coverage_with_weak_cosz_dependency'].values
    Sfcprop['fice']   = ds_columns['ice_fraction_over_open_water'].values
    Sfcprop['tisfc']  = ds_columns['surface_temperature_over_ice_fraction'].values

    ## Get sigma levels
    ak = ds['atmosphere_hybrid_a_coordinate'].values
    bk = ds['atmosphere_hybrid_b_coordinate'].values

    sigma = ((ak + p_ref*bk - ak[0]) / (p_ref - ak[0]))[::-1]
    
    ## Getting random numbers 
    np.random.seed(10)
    randomdict = {}
    # swrad ngptsw = 112
    randomdict['sw_rand'] = np.random.rand(ncolumns,nz*112) 
    # lwrad ngptlw  = 140
    randomdict['lw_rand'] = np.random.rand(ncolumns,nz*140)

    return Statein, Grid, Sfcprop, sigma, randomdict 

def rename_fields(Radtendout, Diagout):
    Radtendout_new = {}
    Radtendout_new['clear_sky_downward_longwave_flux_at_surface']  = Radtendout["sfcflw"]['dnfx0']
    Radtendout_new['clear_sky_downward_shortwave_flux_at_surface'] = Radtendout["sfcfsw"]['dnfx0']
    Radtendout_new['clear_sky_upward_longwave_flux_at_surface'] = Radtendout["sfcflw"]['upfx0']
    Radtendout_new['clear_sky_upward_longwave_flux_at_top_of_atmosphere'] =  Diagout["topflw"]["upfx0"]
    Radtendout_new['clear_sky_upward_shortwave_flux_at_surface'] =  Radtendout["sfcfsw"]['upfx0']
    Radtendout_new['clear_sky_upward_shortwave_flux_at_top_of_atmosphere'] = Diagout["topfsw"]["upfx0"]
    Radtendout_new['total_sky_downward_longwave_flux_at_surface'] =  Radtendout["sfcflw"]['dnfxc']
    Radtendout_new['total_sky_downward_shortwave_flux_at_top_of_atmosphere'] =  Diagout["topfsw"]["dnfxc"]
    Radtendout_new['total_sky_upward_longwave_flux_at_surface'] =  Radtendout["sfcflw"]['upfxc']
    Radtendout_new['total_sky_upward_longwave_flux_at_top_of_atmosphere'] =  Diagout["topflw"]['upfxc']
    Radtendout_new['total_sky_upward_shortwave_flux_at_surface'] =  Radtendout["sfcfsw"]['upfxc']
    Radtendout_new['total_sky_upward_shortwave_flux_at_top_of_atmosphere'] = Diagout["topfsw"]['upfxc']
               
    return Radtendout_new