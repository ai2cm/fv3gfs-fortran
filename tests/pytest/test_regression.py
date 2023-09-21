import copy
import datetime
import glob
import os
from os.path import join
from pathlib import Path
import pytest
import shutil
import subprocess
import fv3config
import numpy as np
import xarray
import typing
import hashlib

import re
import prescribed_ssts


EMULATION_DEBUG_MODE_ISSUE = (
    "We do not build the fortran model in debug mode with call_py_fort, because "
    "it leads to errors even in non-emulation cases.  This means that we cannot "
    "run emulation-related tests in debug mode.  See GitHub issue #365 for more "
    "details."
)
RESTART_REPRODUCIBILITY_DEBUG_MODE_ISSUE = (
    "The model does not restart reproducibly when compiled in debug mode, due to "
    "the -finit-logical=true compiler flag.  If this flag is removed and all "
    "other debug-mode compiler flags are retained, the model restarts "
    "reproducibly.  See GitHub issue #381 for more details."
)
TEST_DIR = os.path.dirname(os.path.realpath(__file__))
CONFIG_DIR = os.path.join(TEST_DIR, "config")


def get_config(filename):
    config_filename = os.path.join(CONFIG_DIR, filename)
    with open(config_filename, "r") as f:
        return fv3config.load(f)


def parse_compile_mode(path):
    name = path.name
    return re.search(r"fv3\.(.*?)\.exe", name).group(1)


def get_executables():
    root = Path(__file__).parent.parent.parent / "bin"
    executable_paths = root.glob("*.exe")
    executables = {}
    for executable_path in executable_paths:
        compile_mode = parse_compile_mode(executable_path)
        executables[compile_mode] = executable_path
    return executables


EXECUTABLES = get_executables()


def run_executable(executable: Path, config: dict, run_dir: str, error_expected: bool = False):
    fv3config.write_run_directory(config, run_dir)
    n_processes = fv3config.config.get_n_processes(config)
    completed_process = subprocess.run(
        ["mpirun", "-n", f"{n_processes}", executable.absolute().as_posix()],
        cwd=run_dir,
        capture_output=True,
    )
    if completed_process.returncode != 0 and not error_expected:
        print("Tail of Stderr:")
        print(completed_process.stderr[-2000:].decode())
        print("Tail of Stdout:")
        print(completed_process.stdout[-2000:].decode())
        pytest.fail()
    return completed_process


@pytest.fixture(scope="session", params=EXECUTABLES.keys())
def executable(request):
    return EXECUTABLES[request.param]


@pytest.mark.parametrize(
    ("config_filename", "check_layout_invariance"),
    [
        pytest.param("default.yml", False, marks=pytest.mark.basic),
        pytest.param("model-level-coarse-graining.yml", True, marks=pytest.mark.coarse),
        pytest.param("pressure-level-coarse-graining.yml", True, marks=pytest.mark.coarse),
        pytest.param("pressure-level-extrapolate-coarse-graining.yml", True, marks=pytest.mark.coarse),
        ("baroclinic.yml", False),
        ("restart.yml", False),
        pytest.param("blended-area-weighted-coarse-graining.yml", True, marks=pytest.mark.coarse)
    ],
)
def test_regression(executable: Path, config_filename: str, check_layout_invariance: bool, tmpdir, regtest):
    config = get_config(config_filename)
    rundir = tmpdir.join("rundir")
    run_executable(executable, config, str(rundir))
    _checksum_rundir(str(rundir), file=regtest)

    if check_layout_invariance:
        config_modified_layout = get_config(config_filename)
        config_modified_layout["namelist"]["fv_core_nml"]["layout"] = [1, 2]
        rundir_modified_layout = tmpdir.join("rundir-modified-layout")
        run_executable(executable, config_modified_layout, rundir_modified_layout)

        expected_checksums = _checksum_restart_files_and_diagnostics(rundir)
        result_checksums = _checksum_restart_files_and_diagnostics(rundir_modified_layout)

        assert result_checksums == expected_checksums

        shutil.rmtree(rundir_modified_layout)
    shutil.rmtree(rundir)

@pytest.mark.parametrize(
    "config_filename",
    [
        pytest.param("default.yml", marks=pytest.mark.basic),
        pytest.param("emulation.yml", marks=pytest.mark.emulation),
        "restart.yml"
    ],
)
def test_restart_reproducibility(executable, config_filename, tmpdir):
    if config_filename == "emulation.yml" and "debug" in str(executable):
        pytest.skip(EMULATION_DEBUG_MODE_ISSUE)

    if "debug" in str(executable):
        pytest.skip(RESTART_REPRODUCIBILITY_DEBUG_MODE_ISSUE)

    config_template = get_config(config_filename)
    config_template["diag_table"] = "no_output"
    config_template["namelist"]["gfs_physics_nml"]["fhswr"] = 900
    config_template["namelist"]["gfs_physics_nml"]["fhlwr"] = 900

    segmented_config = copy.deepcopy(config_template)
    continuous_config = copy.deepcopy(config_template)

    duration = datetime.timedelta(minutes=30)
    segmented_config = fv3config.set_run_duration(segmented_config, duration // 2)
    continuous_config = fv3config.set_run_duration(continuous_config, duration)

    segment_1_rundir = str(tmpdir.join("segment-1"))
    segment_2_rundir = str(tmpdir.join("segment-2"))
    continuous_rundir = str(tmpdir.join("continuous"))

    run_executable(executable, segmented_config, segment_1_rundir)
    run_executable(executable, continuous_config, continuous_rundir)

    segment_1_restarts = os.path.join(segment_1_rundir, "RESTART")
    segmented_config = fv3config.enable_restart(segmented_config, segment_1_restarts)
    run_executable(executable, segmented_config, segment_2_rundir)

    continuous_checksums = _checksum_restart_files(continuous_rundir)
    segmented_checksums = _checksum_restart_files(segment_2_rundir)

    assert segmented_checksums == continuous_checksums
    shutil.rmtree(segment_1_rundir)
    shutil.rmtree(segment_2_rundir)
    shutil.rmtree(continuous_rundir)


def test_indefinite_physics_diagnostics(executable, tmpdir):
    config_template = get_config("default.yml")

    fdiag = copy.deepcopy(config_template)
    fdiag["namelist"]["atmos_model_nml"]["fhout"] = 0.5
    fdiag["namelist"]["atmos_model_nml"]["use_fdiag"] = True

    indefinite = copy.deepcopy(config_template)
    indefinite["namelist"]["atmos_model_nml"]["fhout"] = 0.5
    # No change is required to the fhmax parameter here, but this is just to
    # demonstrate that with the use_fdiag = .false. option, the value of fhmax is
    # ignored and physics diagnostics are output indefinitely.
    indefinite["namelist"]["atmos_model_nml"]["fhmax"] = 0.0

    fdiag_rundir = str(tmpdir.join("fdiag"))
    indefinite_rundir = str(tmpdir.join("indefinite"))
    run_executable(executable, fdiag, fdiag_rundir)
    run_executable(executable, indefinite, indefinite_rundir)

    fdiag_checksums = _checksum_diagnostics(fdiag_rundir)
    indefinite_checksums = _checksum_diagnostics(indefinite_rundir)
    assert fdiag_checksums == indefinite_checksums
    shutil.rmtree(fdiag_rundir)
    shutil.rmtree(indefinite_rundir)


def open_tiles(prefix):
    files = [f"{prefix}.tile{tile}.nc" for tile in range(1, 7)]
    datasets = []
    for file in files:
        ds = xarray.open_dataset(file)
        datasets.append(ds)
    return xarray.concat(datasets, dim="tile")


def test_use_prescribed_sea_surface_properties(executable, tmpdir):
    config = get_config("default.yml")

    prescribed_ssts.create_sst_dataset(tmpdir)
    patch_files = prescribed_ssts.get_patch_files(tmpdir)
    config["patch_files"] = patch_files
    config["namelist"]["gfs_physics_nml"]["use_prescribed_sea_surface_properties"] = True
    config["namelist"]["fv_grid_nml"]["grid_file"] = "INPUT/grid_spec.nc"

    rundir = os.path.join(str(tmpdir), "rundir")
    run_executable(executable, config, rundir)

    results = open_tiles(os.path.join(rundir, "sfc_dt_atmos"))
    prescribed_ssts.validate_ssts(results)
    shutil.rmtree(rundir)


PRESCRIBED_SST_ERRORS = {
    "MPP_OPEN:INPUT/sst.nc does not exist.": prescribed_ssts.grid_file_assets("C12")
    + [prescribed_ssts.data_table_asset()],
    "sea_surface_temperature dataset not specified in data_table.": prescribed_ssts.grid_file_assets("C12"),
}


@pytest.mark.parametrize(
    ("message", "patch_files"),
    list(PRESCRIBED_SST_ERRORS.items()),
    ids=list(PRESCRIBED_SST_ERRORS.keys()),
)
def test_use_prescribed_sea_surface_properties_error(executable, tmpdir, message, patch_files):
    config = get_config("default.yml")
    config["patch_files"] = patch_files
    config["namelist"]["gfs_physics_nml"]["use_prescribed_sea_surface_properties"] = True
    config["namelist"]["fv_grid_nml"]["grid_file"] = "INPUT/grid_spec.nc"
    rundir = os.path.join(str(tmpdir), "rundir")
    result = run_executable(executable, config, rundir, error_expected=True)
    assert message in result.stderr.decode()
    shutil.rmtree(rundir)


@pytest.fixture(scope="session")
def emulation_run(executable, tmpdir_factory):
    if "debug" in str(executable):
        pytest.skip(EMULATION_DEBUG_MODE_ISSUE)

    config = get_config("emulation.yml")
    rundir = tmpdir_factory.mktemp("rundir")
    run_dir = str(rundir)
    completed_process = run_executable(executable, config, run_dir)
    return completed_process, run_dir


@pytest.mark.emulation
def test_callpyfort_integration(emulation_run):
    _, run_dir = emulation_run
    assert os.path.exists(join(run_dir, "microphysics_success.txt"))
    assert os.path.exists(join(run_dir, "store_success.txt"))


@pytest.mark.emulation
@pytest.mark.parametrize("tile", range(1, 7))
def test_zhao_carr_diagnostics(emulation_run, regtest, tile):
    rundir = emulation_run[1]
    ds = xarray.open_dataset(os.path.join(rundir, f"piggy.tile{tile}.nc"))
    # print schema to regression data
    ds.info(regtest)


@pytest.mark.emulation
def test_gscond_logs(executable, regtest, tmpdir):
    if "debug" in str(executable):
        pytest.skip(EMULATION_DEBUG_MODE_ISSUE)

    config = get_config("emulation.yml")
    config["namelist"]["gfs_physics_nml"]["emulate_gscond_only"] = True
    rundir = tmpdir.join("rundir")
    process = run_executable(executable, config, str(rundir))
    gscond_state_info = re.findall(r"gscond.state:(.*)", process.stderr.decode())
    first_state = gscond_state_info[0]
    print(first_state, file=regtest)


@pytest.mark.emulation
@pytest.mark.parametrize("tile", range(1, 7))
def test_zhao_carr_surface_precipitation_matches_total_water_source(
    emulation_run, tile
):
    """The column integrated water sink roughly matches the surface
    precipitation in the Zhao-carr scheme.

    Large relative changes indicate a problem with the surface precipitation
    diagnostic
    """
    _, rundir = emulation_run
    ds = xarray.open_dataset(os.path.join(rundir, f"piggy.tile{tile}.nc"))

    total_water_source = (
        ds.tendency_of_cloud_water_due_to_zhao_carr_physics
        + ds.tendency_of_specific_humidity_due_to_zhao_carr_physics
    )

    precip = ds.surface_precipitation_due_to_zhao_carr_physics

    column_water_source = (total_water_source * ds.delp).sum("pfull") / 9.81

    def rms(x):
        return np.sqrt((x ** 2).mean().item())

    rms_column_water_source = rms(column_water_source)
    rms_precip = rms(precip)
    assert rms_precip == pytest.approx(rms_column_water_source, rel=0.1)


def checksum_file(path: str) -> str:
    sum = hashlib.md5()
    BUFFER_SIZE = 1024 * 1024
    with open(path, "rb") as f:
        while True:
            buf = f.read(BUFFER_SIZE)
            if not buf:
                break
            sum.update(buf)
    return sum.hexdigest()


def _checksum_restart_files(rundir: str) -> typing.Dict[str, str]:
    restart_files = sorted(glob.glob(os.path.join(rundir, "RESTART", "*.nc")))
    return {os.path.basename(file): checksum_file(file) for file in restart_files}


def _checksum_diagnostics(rundir: str):
    files = glob.glob(os.path.join(rundir, "*.nc"))
    return {os.path.basename(file): checksum_file(file) for file in files}


def _checksum_restart_files_and_diagnostics(rundir: str):
    checksums = {}
    checksums["restart_files"] = _checksum_restart_files(rundir)
    checksums["diagnostics"] = _checksum_diagnostics(rundir)
    return checksums


def _checksum_rundir(rundir: str, file):
    """checksum rundir storing output in file"""
    files = glob.glob(os.path.join(rundir, "*.nc"))
    restart_files = glob.glob(os.path.join(rundir, "RESTART", "*.nc"))
    for path in sorted(files) + sorted(restart_files):
        print(path, checksum_file(path), file=file)


@pytest.mark.emulation
def test_checksum_emulation(emulation_run, regtest):
    _, run_dir = emulation_run
    _checksum_rundir(run_dir, file=regtest)


if __name__ == "__main__":
    pytest.main()
