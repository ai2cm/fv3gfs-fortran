# This code shows an example where we save a variables obtained from get_state() into a netcdf file 
# Run using mpirun -n 6 python3 get_hprime.py
from datetime import timedelta
import xarray as xr
import numpy as np



dirIn = '/home/yakelyn/fv3gfs-fortran/FV3/wrapper/rundirectory2/'
qgrs = np.loadtxt(dirIn + 'file_qgrs.dat', unpack = True)

names = ['tsfc', 'slmsk','snowd','sncovr','snoalb','zorl','hprime','alvsf',
'alnsf','alvwf','alnwf','facsf','facwf','fice','tisfc']
dirIn = '/home/yakelyn/fv3gfs-fortran/FV3/wrapper/rundirectory/'


for n in range(len(names)):
    print(names[n])
    sfcprop = np.load(dirIn + names[n] +'_0.npz')['arr_0'][0]
    print(sfcprop)
dirIn = '/home/yakelyn/fv3gfs-fortran/FV3/wrapper/rundirectory2/'
file =  dirIn + 'inputs_0_2016_8_5_0_15.nc'
ds_inputs = xr.open_dataset(file)



# prsi_wrapper = preprocess.pressure_at_interface(inputs['pressure_thickness_of_atmospheric_layer'])
# prsl_log_wrapper = preprocess.pressure_at_midpoint_log(inputs['pressure_thickness_of_atmospheric_layer'],'z')

# tsfc_wrapper = inputs['surface_temperature'].values



dirIn = '/home/yakelyn/fv3gfs-fortran/FV3/wrapper/rundirectory/'
tracer = np.loadtxt(dirIn + 'file_tracer.dat', unpack = True)

for n in range(8):
    diff = qgrs[n,:] - tracer[n+1,:]
    print(np.nanmax(diff))