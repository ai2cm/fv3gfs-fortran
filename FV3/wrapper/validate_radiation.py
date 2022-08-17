# This code shows an example where we save a variables obtained from get_state() into a netcdf file 
# Run using mpirun -n 6 python3 get_hprime.py
from datetime import timedelta
from mpi4py import MPI
import fv3gfs.wrapper
import yaml
import xarray as xr
import numpy as np
import preprocess 
this_variables = preprocess.this_variables
to_validate = preprocess.to_validate

## needs importing the radiation module 
PORT_DIR = '/home/yakelyn/fv3net/external/radiation/python'
import sys
sys.path.append(PORT_DIR)
import getdata
from radiation_driver import RadiationDriver

FORCING_DIR = '/home/yakelyn/fv3net/external/radiation/python/forcing/'
LOOKUP_DIR  = '/home/yakelyn/fv3net/external/radiation/python/lookupdata/' 

## needs the configuration yaml file to get radinit and 
RAD_CONFIG_FILE = '/home/yakelyn/fv3gfs-fortran/FV3/wrapper/rad_static_config.yaml'
with open(RAD_CONFIG_FILE, 'r') as f:
    config_radiation = yaml.safe_load(f)
Model = config_radiation['model']


## Static data
# reading datasets needed for radinit() and radupdate()
me = 0
aer_dict = getdata.aerosol(FORCING_DIR)
solar_filename, solar_data = getdata.astronomy(FORCING_DIR, config_radiation['rad_init']['isolar'], me)
sfc_file, sfc_data = getdata.sfc(FORCING_DIR)
gas_data = getdata.gases(FORCING_DIR, config_radiation['rad_init']['ictmflg'])
lwdict = getdata.lw(LOOKUP_DIR)
swdict = getdata.sw(LOOKUP_DIR)


rundir_basename = "rundir"


if __name__ == "__main__":
    comm = MPI.COMM_WORLD
    rank = comm.Get_rank()
    if rank == 0:  # only use filesystem on one rank
        with open("fv3config.yml", "r") as config_file:
            config = yaml.safe_load(config_file)
        config = comm.bcast(config)
    else:
        config = comm.bcast(None)
    
    fv3gfs.wrapper.initialize()
    
    for i in range(fv3gfs.wrapper.get_step_count()):
        fv3gfs.wrapper.step_dynamics()
        fv3gfs.wrapper.step_pre_radiation()

        time  = fv3gfs.wrapper.get_state(names=['time'])

        ## inputs needed for rad port
        ds_inputs = xr.Dataset()
        for varname in this_variables:
            state = fv3gfs.wrapper.get_state(names=[varname])
            tmp   = state[varname].data_array.to_dataset(name=varname)
            ds_inputs = xr.combine_by_coords([ds_inputs, tmp],compat='override') 

        Model['me']   =  i   
        Model['levs'] = ds_inputs['air_temperature'].z.size
        Model['levr'] = ds_inputs['air_temperature'].z.size
        nlay = ds_inputs['air_temperature'].z.size

        Statein, Grid, Sfcprop, sigma, randomdict  = preprocess.get_radiation_inputs(ds_inputs)
       
        if i == 0:
            ## Calling radiation 
            driver = RadiationDriver()
            ## intialize
            driver.radinit(
                sigma,
                nlay,
                config_radiation['rad_init']['imp_physics'],
                i,
                config_radiation['rad_init']['iemsflg'],
                config_radiation['rad_init']['ioznflg'],
                config_radiation['rad_init']['ictmflg'],
                config_radiation['rad_init']['isolar'],
                config_radiation['rad_init']['ico2flg'],
                config_radiation['rad_init']['iaerflg'],
                config_radiation['rad_init']['ialbflg'],
                config_radiation['rad_init']['icldflg'],
                config_radiation['rad_init']['ivflip'],
                config_radiation['rad_init']['iovrsw'],
                config_radiation['rad_init']['iovrlw'],
                config_radiation['rad_init']['isubcsw'],
                config_radiation['rad_init']['isubclw'],
                config_radiation['rad_init']['lcrick'],
                config_radiation['rad_init']['lcnorm'],
                config_radiation['rad_init']['lnoprec'],
                config_radiation['rad_init']['iswcliq'],
                aer_dict,
                solar_filename,
                sfc_file,
                sfc_data
                )

            updatedict= {}
            updatedict['idat']   = np.array([time['time'].year, time['time'].month, time['time'].day, time['time'].hour, time['time'].minute, 0, 0 ,0])  
            updatedict['jdat']   = np.array([time['time'].year, time['time'].month, time['time'].day, time['time'].hour, time['time'].minute, 0, 0 ,0])
            updatedict['fhswr']  = np.array([config['namelist']['gfs_physics_nml']['fhswr']])
            updatedict['dtf']    = np.array([config['namelist']['coupler_nml']['dt_atmos']])
            updatedict['lsswr']  = True

            slag, sdec, cdec, solcon = driver.radupdate(
                updatedict["idat"],
                updatedict["jdat"],
                updatedict["fhswr"],
                updatedict["dtf"],
                updatedict["lsswr"],
                aer_dict["kprfg"],
                aer_dict["idxcg"],
                aer_dict["cmixg"],
                aer_dict["denng"],
                aer_dict["cline"],
                solar_data,
                gas_data)


        else:
            updatedict= {}
            updatedict['idat']   = np.array([time['time'].year, time['time'].month, time['time'].day, time['time'].hour, time['time'].minute, 0, 0 ,0])  
            updatedict['jdat']   = np.array([time['time'].year, time['time'].month, time['time'].day, time['time'].hour, time['time'].minute, 0, 0 ,0])
            updatedict['fhswr']  = np.array([config['namelist']['gfs_physics_nml']['fhswr']])
            updatedict['dtf']    = np.array([config['namelist']['coupler_nml']['dt_atmos']])
            updatedict['lsswr']  = True

            slag, sdec, cdec, solcon = driver.radupdate(
                updatedict["idat"],
                updatedict["jdat"],
                updatedict["fhswr"],
                updatedict["dtf"],
                updatedict["lsswr"],
                aer_dict["kprfg"],
                aer_dict["idxcg"],
                aer_dict["cmixg"],
                aer_dict["denng"],
                aer_dict["cline"],
                solar_data,
                gas_data)

        print(' ############ Updatedict ##########')
        print(updatedict)

        Radtendout, Diagout, Couplingout = driver._GFS_radiation_driver(Model, Statein, Sfcprop, Grid, randomdict, lwdict, swdict)

        valdict= preprocess.rename_fields(Radtendout, Diagout)
        
        
        fv3gfs.wrapper.step_radiation()
        ds_outputs_post_radiation = xr.Dataset()
        for varname in to_validate:
            state = fv3gfs.wrapper.get_state(names=[varname])
            tmp   = state[varname].data_array.to_dataset(name=varname)
            ds_outputs_post_radiation = xr.combine_by_coords([ds_outputs_post_radiation, tmp],compat='override') 
        outdict  = preprocess.stack_ds(ds_outputs_post_radiation)
        
        for var in valdict:
            diff = np.nanmax(valdict[var] - outdict[var])
            print('max difference for ' + var + '  =  ' + str(diff))
        
        #preprocess.compare_data(valdict, outdict)
        #fv3gfs.wrapper.step_post_radiation_physics()
    fv3gfs.wrapper.cleanup()



