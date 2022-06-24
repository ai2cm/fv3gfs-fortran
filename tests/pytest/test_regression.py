import copy
import datetime
import glob
import os
from os.path import join
import shutil
import pytest
import fv3config
import numpy as np
import xarray
import subprocess
import typing
import hashlib

import re
import prescribed_ssts


TEST_DIR = os.path.dirname(os.path.realpath(__file__))
REFERENCE_DIR = os.path.join(TEST_DIR, "reference")
OUTPUT_DIR = os.path.join(TEST_DIR, "output")
CONFIG_DIR = os.path.join(TEST_DIR, "config")
SUBMIT_JOB_FILENAME = os.path.join(TEST_DIR, "run_files/submit_job.sh")
STDOUT_FILENAME = "stdout.log"
STDERR_FILENAME = "stderr.log"
MD5SUM_FILENAME = "md5.txt"
SERIALIZE_MD5SUM_FILENAME = "md5_serialize.txt"
GOOGLE_APP_CREDS = os.environ.get("GOOGLE_APPLICATION_CREDENTIALS", None)

USE_LOCAL_ARCHIVE = True

config_filenames = os.listdir(CONFIG_DIR)


@pytest.fixture
def image_version(request):
    return request.config.getoption("--image_version")


@pytest.fixture
def image(request):
    return request.config.getoption("--image")


@pytest.fixture
def reference_dir(request):
    return request.config.getoption("--refdir")


@pytest.fixture
def code_root(request):
    return request.config.getoption("--code_root")


@pytest.fixture
def image_runner(request):
    if request.config.getoption("--native"):
        pytest.skip()
    return request.config.getoption("--image_runner")


def get_config(filename):
    config_filename = os.path.join(CONFIG_DIR, filename)
    with open(config_filename, "r") as f:
        return fv3config.load(f)


def get_run_dir(model_image_tag, config):
    run_name = config["experiment_name"]
    return os.path.join(OUTPUT_DIR, model_image_tag, run_name)


def get_n_processes(config):
    layout = config["namelist"]["fv_core_nml"]["layout"]
    return 6 * layout[0] * layout[1]


@pytest.mark.parametrize(
    ("config_filename", "tag"),
    [
        ("default.yml", "{version}-debug"),
        ("baroclinic.yml", "{version}-debug"),
        ("default.yml", "{version}"),
        ("default.yml", "{version}-serialize"),
        ("restart.yml", "{version}"),
        ("model-level-coarse-graining.yml", "{version}-debug"),
        ("pressure-level-coarse-graining.yml", "{version}-debug"),
    ],
)
def test_regression(
    config_filename, tag, image, image_version, reference_dir, image_runner
):
    model_image_tag = tag.format(version=image_version)
    model_image = f"{image}:{model_image_tag}"
    config = get_config(config_filename)
    run_dir = get_run_dir(model_image_tag, config)
    run_model(config, run_dir, model_image, image_runner)
    run_name = config["experiment_name"]
    run_reference_dir = os.path.join(reference_dir, run_name)
    md5sum_filename = os.path.join(run_reference_dir, MD5SUM_FILENAME)
    check_rundir_md5sum(run_dir, md5sum_filename)
    if "serialize" in model_image:
        serialize_md5sum_filename = os.path.join(
            run_reference_dir, SERIALIZE_MD5SUM_FILENAME
        )
        check_rundir_md5sum(run_dir, serialize_md5sum_filename)
    shutil.rmtree(run_dir)


@pytest.mark.parametrize(
    "config_filename",
    [
        "default.yml",
        "baroclinic.yml",
        "restart.yml",
        "model-level-coarse-graining.yml",
        "pressure-level-coarse-graining.yml",
    ],
)
def test_regression_native(run_native, config_filename: str, tmpdir, system_regtest):
    config = get_config(config_filename)
    rundir = tmpdir.join("rundir")
    run_native(config, str(rundir))
    _checksum_rundir(str(rundir), file=system_regtest)


@pytest.mark.parametrize(
    "config_filename", ["default.yml", "emulation.yml", "restart.yml"]
)
def test_restart_reproducibility(run_native, config_filename, tmpdir):
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

    run_native(segmented_config, segment_1_rundir)
    run_native(continuous_config, continuous_rundir)

    segment_1_restarts = os.path.join(segment_1_rundir, "RESTART")
    segmented_config = fv3config.enable_restart(segmented_config, segment_1_restarts)
    run_native(segmented_config, segment_2_rundir)

    continuous_checksums = _checksum_restart_files(continuous_rundir)
    segmented_checksums = _checksum_restart_files(segment_2_rundir)

    assert segmented_checksums == continuous_checksums


def test_indefinite_physics_diagnostics(run_native, tmpdir):
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
    run_native(fdiag, fdiag_rundir)
    run_native(indefinite, indefinite_rundir)

    fdiag_checksums =  _checksum_diagnostics(fdiag_rundir)
    indefinite_checksums = _checksum_diagnostics(indefinite_rundir)
    assert fdiag_checksums == indefinite_checksums


def open_tiles(prefix):
    files = [f"{prefix}.tile{tile}.nc" for tile in range(1, 7)]
    datasets = []
    for file in files:
        ds = xarray.open_dataset(file)
        datasets.append(ds)
    return xarray.concat(datasets, dim="tile")


def test_use_prescribed_sea_surface_properties(run_native, tmpdir):
    config = get_config("default.yml")

    prescribed_ssts.create_sst_dataset(tmpdir)
    patch_files = prescribed_ssts.get_patch_files(tmpdir)
    config["patch_files"] = patch_files
    config["namelist"]["gfs_physics_nml"]["use_prescribed_sea_surface_properties"] = True
    config["namelist"]["fv_grid_nml"]["grid_file"] = "INPUT/grid_spec.nc"

    rundir = os.path.join(str(tmpdir), "rundir")
    run_native(config, rundir)

    results = open_tiles(os.path.join(rundir, "sfc_dt_atmos"))
    prescribed_ssts.validate_ssts(results)


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
def test_use_prescribed_sea_surface_properties_error(run_native, tmpdir, message, patch_files):
    config = get_config("default.yml")
    config["patch_files"] = patch_files
    config["namelist"]["gfs_physics_nml"]["use_prescribed_sea_surface_properties"] = True
    config["namelist"]["fv_grid_nml"]["grid_file"] = "INPUT/grid_spec.nc"
    rundir = os.path.join(str(tmpdir), "rundir")
    result = run_native(config, rundir, error_expected=True)
    assert message in result.stderr.decode()


@pytest.fixture(scope="session")
def emulation_run(run_native, tmpdir_factory):
    config = get_config("emulation.yml")
    rundir = tmpdir_factory.mktemp("rundir")
    run_dir = str(rundir)
    completed_process = run_native(config, run_dir)
    return completed_process, run_dir


def test_callpyfort_integration(emulation_run):
    _, run_dir = emulation_run
    assert os.path.exists(join(run_dir, "microphysics_success.txt"))
    assert os.path.exists(join(run_dir, "store_success.txt"))


@pytest.mark.parametrize("tile", range(1, 7))
def test_zhao_carr_diagnostics(emulation_run, regtest, tile):
    rundir = emulation_run[1]
    ds = xarray.open_dataset(os.path.join(rundir, f"piggy.tile{tile}.nc"))
    # print schema to regression data
    ds.info(regtest)


def test_gscond_logs(run_native, regtest, tmpdir):
    config = get_config("emulation.yml")
    config["namelist"]["gfs_physics_nml"]["emulate_gscond_only"] = True
    rundir = tmpdir.join("rundir")
    process = run_native(config, str(rundir))
    gscond_state_info = re.findall(r"gscond.state:(.*)", process.stderr.decode())
    first_state = gscond_state_info[0]
    print(first_state, file=regtest)


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


def _checksum_rundir(rundir: str, file):
    """checksum rundir storing output in file"""
    files = glob.glob(os.path.join(rundir, "*.nc"))
    restart_files = glob.glob(os.path.join(rundir, "RESTART", "*.nc"))
    for path in sorted(files) + sorted(restart_files):
        print(path, checksum_file(path), file=file)


def test_checksum_emulation(emulation_run, system_regtest):
    _, run_dir = emulation_run
    _checksum_rundir(run_dir, file=system_regtest)


def check_rundir_md5sum(run_dir, md5sum_filename):
    ensure_reference_exists(md5sum_filename)
    subprocess.check_call(["md5sum", "-c", md5sum_filename], cwd=run_dir)


def ensure_reference_exists(filename):
    if not os.path.isfile(filename):
        raise AssertionError(
            f"reference md5sum does not exist at " + filename + ","
            f" you can create it with `set_reference.sh` -- refer to README.md"
        )


def run_model_docker(rundir, model_image, n_processes, additional_env_vars=None):
    if USE_LOCAL_ARCHIVE:
        archive = fv3config.get_cache_dir()
        archive_mount = ["-v", f"{archive}:{archive}"]
    else:
        archive_mount = []

    if GOOGLE_APP_CREDS is not None:
        secret_mount = ["-v", f"{GOOGLE_APP_CREDS}:/tmp/key.json"]
        env_vars = ["--env", "GOOGLE_APPLICATION_CREDENTIALS"]
    else:
        secret_mount = []
        env_vars = []

    if additional_env_vars is not None:
        env_vars += additional_env_vars

    docker_runpath = ""
    docker_run = ["docker", "run", "--rm"]
    rundir_abs = os.path.abspath(rundir)
    rundir_mount = ["-v", f"{rundir_abs}:" + docker_runpath + "/rundir"]
    data_abs = os.path.abspath(os.path.join(rundir_abs, "test_data"))
    os.makedirs(data_abs, exist_ok=True)
    data_mount = ["-v", f"{data_abs}:" + docker_runpath + "/rundir/test_data"]
    fv3out_filename = join(rundir, "stdout.log")
    fv3err_filename = join(rundir, "stderr.log")
    call = (
        docker_run
        + rundir_mount
        + archive_mount
        + data_mount
        + secret_mount
        + env_vars
        + [model_image]
        + ["bash", "/rundir/submit_job.sh", str(n_processes)]
    )
    with open(fv3out_filename, "w") as fv3out_f, open(fv3err_filename, "w") as fv3err_f:
        subprocess.check_call(
            call,
            stdout=fv3out_f,
            stderr=fv3err_f,
        )


def run_model_sarus(rundir, model_image, n_processes):
    shutil.copy(
        os.path.join(TEST_DIR, "run_files/job_jenkins_sarus"),
        os.path.join(rundir, "job_jenkins_sarus"),
    )
    # run job_jenkins_sarus with env var FV3_CONTAINER set to model_image
    env = os.environ.copy()
    env["FV3_CONTAINER"] = model_image
    env["SCRATCH_DIR"] = rundir
    call = ["sbatch", "--wait", f"--ntasks={n_processes}", "job_jenkins_sarus"]
    subprocess.check_call(call, env=env, cwd=rundir)


def check_md5sum(run_dir, md5sum_filename):
    subprocess.check_call(["md5sum", "-c", md5sum_filename], cwd=run_dir)


def write_run_directory(config, dirname):
    fv3config.write_run_directory(config, dirname)
    shutil.copy(SUBMIT_JOB_FILENAME, os.path.join(dirname, "submit_job.sh"))


def run_model(config, run_dir, model_image, image_runner, additional_env_vars=None):
    if os.path.isdir(run_dir):
        shutil.rmtree(run_dir)
    os.makedirs(run_dir)
    write_run_directory(config, run_dir)
    n_processes = get_n_processes(config)
    if image_runner == "docker":
        run_model_docker(
            run_dir, model_image, n_processes, additional_env_vars=additional_env_vars
        )
    elif image_runner == "sarus":
        run_model_sarus(run_dir, model_image, n_processes)
    else:
        raise NotImplementedError("image_runner must be one of 'docker' or 'sarus'")


@pytest.mark.parametrize(
    ("config_filename", "tag", "layout"),
    [
        ("model-level-coarse-graining.yml", "{version}", [1, 2]),
        ("pressure-level-coarse-graining.yml", "{version}", [1, 2]),
    ],
    ids=lambda x: str(x),
)
def test_run_reproduces_across_layouts(
    config_filename, tag, layout, image, image_version, image_runner, reference_dir
):
    model_image_tag = tag.format(version=image_version)
    model_image = f"{image}:{model_image_tag}"
    config = get_config(config_filename)
    config["namelist"]["fv_core_nml"]["layout"] = layout
    layout_x, layout_y = layout
    run_name = f"{config['experiment_name']}_{layout_x}x{layout_y}"
    run_dir = join(OUTPUT_DIR, model_image_tag, run_name)
    run_model(config, run_dir, model_image, image_runner)

    reference_run_name = config["experiment_name"]
    run_reference_dir = join(reference_dir, reference_run_name)
    md5sum_filename = join(run_reference_dir, MD5SUM_FILENAME)
    check_rundir_md5sum(run_dir, md5sum_filename)
    shutil.rmtree(run_dir)


if __name__ == "__main__":
    pytest.main()
