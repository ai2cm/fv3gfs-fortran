import fv3config
import os
from os.path import join
import shutil
from copy import deepcopy
import xarray as xr
import hashlib
import subprocess


def setup_initial_runs(workdir, config_template):
    """Set up two run-directories, one for a four-hour run, one for
    two-hour run, both initialized from the same state example_restart
    state"""
    if not os.path.isdir(workdir):
        os.mkdir(workdir)
    fullrun_config = deepcopy(config_template)
    fullrun_config['namelist']['coupler_nml']['hours'] = 4
    fullrun_config['namelist']['coupler_nml']['minutes'] = 0
    firsthalf_config = deepcopy(fullrun_config)
    firsthalf_config['namelist']['coupler_nml']['hours'] = 2
    fv3config.write_run_directory(fullrun_config, join(workdir, 'fullrun'))
    fv3config.write_run_directory(firsthalf_config, join(workdir, 'firsthalf'))
    for run in ['fullrun', 'firsthalf']:
        shutil.copy('submit_job.sh', join(workdir, run, 'submit_job.sh'))
    return fullrun_config, firsthalf_config


def setup_final_run(workdir, firsthalf_config, remove_phy_data=False):
    """Set up run directory for second two-hour run. This must be done after
    the first two-hour run has been performed so that the initial conditions 
    are linked appropriately"""
    secondhalf_config = deepcopy(firsthalf_config)
    secondhalf_config['initial_conditions'] = os.path.abspath(
        join(workdir, 'firsthalf', 'RESTART')
    )
    secondhalf_config = fv3config.enable_restart(secondhalf_config)
    fv3config.write_run_directory(secondhalf_config, join(workdir, 'secondhalf'))
    if remove_phy_data:
        for tile in range(1, 7):
            os.remove(join(workdir, 'secondhalf', 'INPUT', f'phy_data.tile{tile}.nc'))
    shutil.copy('submit_job.sh', join(workdir, 'secondhalf', 'submit_job.sh'))
    return secondhalf_config


def run_model(rundir, model_image, mounts):
    docker_run = ['docker', 'run', '--rm']
    rundir_abs = os.path.abspath(rundir)
    rundir_mount = ['-v', f'{rundir_abs}:/rundir']
    fv3out_filename = join(rundir, 'fv3out')
    fv3err_filename = join(rundir, 'fv3err')
    with open(fv3out_filename, 'w') as fv3out_f, open(fv3err_filename, 'w') as fv3err_f:
        subprocess.call(docker_run + rundir_mount + mounts + [model_image] + ["bash", "/rundir/submit_job.sh"],
                        stdout=fv3out_f,
                        stderr=fv3err_f)
        
        
def run_full_and_split(workdir, config_template, remove_phy_data=False):
    archive = fv3config.get_cache_dir()
    archive_mount = ['-v', f'{archive}:{archive}']
    model_image = 'fv3gfs-compiled-default'
    _, firsthalf_config = setup_initial_runs(workdir, config_template)
    run_model(join(workdir, 'fullrun'), model_image, archive_mount)
    run_model(join(workdir, 'firsthalf'), model_image, archive_mount)
    _ = setup_final_run(workdir, firsthalf_config, remove_phy_data=remove_phy_data)
    icdir = os.path.abspath(join(workdir, 'firsthalf', 'RESTART'))
    ic_mount = ['-v', f'{icdir}:{icdir}']
    run_model(join(workdir, 'secondhalf'), model_image, archive_mount + ic_mount)


def compare_restart_files(dir1, dir2, verbose=False):
    print(f'Checking diff between restart files in {dir1} and {dir2}.')
    restart_files = sorted(os.listdir(join(dir1, 'RESTART')))
    restart_files.remove('coupler.res')  # don't expect this file to be the same
    file_difference_count = 0
    for file in restart_files:
        with open(join(dir1, 'RESTART', file), 'rb') as f:
            fullrun_hash = hashlib.md5(f.read()).hexdigest()
        with open(join(dir2, 'RESTART', file), 'rb') as f:
            secondhalf_hash = hashlib.md5(f.read()).hexdigest()
        if fullrun_hash != secondhalf_hash:
            file_difference_count +=1
            if verbose:
                print(f'{file} differs')
    print(f'A total of {file_difference_count} restart files differ.')


def get_base_config():
    config = fv3config.get_default_config()
    config['initial_conditions'] = 'restart_example'
    config['namelist']['fv_core_nml']['external_ic'] = False
    config['namelist']['fv_core_nml']['nggps_ic'] = False
    config['namelist']['fv_core_nml']['make_nh'] = False
    config['namelist']['fv_core_nml']['mountain'] = True
    config['namelist']['fv_core_nml']['warm_start'] = True
    config['namelist']['fv_core_nml']['na_init'] = 0
    return config


def test_gfs_standard():
    workdir = 'rundirs_gfs_standard'
    config_standard = fv3config.get_default_config()
    run_full_and_split(workdir, config_standard)
    compare_restart_files(join(workdir, 'fullrun'), join(workdir, 'secondhalf'))


def test_restart_standard():
    workdir = 'rundirs_restart_standard'
    config_standard = get_base_config()
    run_full_and_split(workdir, config_standard)
    compare_restart_files(join(workdir, 'fullrun'), join(workdir, 'secondhalf'))


def test_isubc_sw_lw_zero():
    workdir = 'rundirs_isubc0'
    config_isubc0 = get_base_config()
    config_isubc0['namelist']['gfs_physics_nml']['isubc_sw'] = 0
    config_isubc0['namelist']['gfs_physics_nml']['isubc_lw'] = 0
    run_full_and_split(workdir, config_isubc0)
    compare_restart_files(join(workdir, 'fullrun'), join(workdir, 'secondhalf'))


def test_dycore_only():
    workdir = 'rundirs_dycore_only'
    config_dycore_only = get_base_config()
    config_dycore_only['namelist']['atmos_model_nml']['dycore_only'] = True
    run_full_and_split(workdir, config_dycore_only)
    compare_restart_files(join(workdir, 'fullrun'), join(workdir, 'secondhalf'))


if __name__ == '__main__':
    test_dycore_only()
    test_gfs_standard()
    test_restart_standard()
    test_isubc_sw_lw_zero()
