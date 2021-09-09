import os
import yaml
import shutil
import subprocess
import pytest
import fv3config
from os.path import join


TEST_DIR = os.path.dirname(os.path.realpath(__file__))
REFERENCE_DIR = os.path.join(TEST_DIR, 'reference')
OUTPUT_DIR = os.path.join(TEST_DIR, 'output')
CONFIG_DIR = os.path.join(TEST_DIR, 'config')
SUBMIT_JOB_FILENAME = os.path.join(TEST_DIR, 'run_files/submit_job.sh')
STDOUT_FILENAME = 'stdout.log'
STDERR_FILENAME = 'stderr.log'
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
    return request.config.getoption("--image_runner")


def get_config(filename):
    config_filename = os.path.join(CONFIG_DIR, filename)
    with open(config_filename, 'r') as config_file:
        return yaml.safe_load(config_file)


def get_run_dir(model_image_tag, config):
    run_name = config['experiment_name']
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
        ("pressure-level-coarse-graining.yml", "{version}-debug")
    ]
)
def test_regression(
    config_filename,
    tag,
    image,
    image_version,
    reference_dir,
    image_runner
):
    model_image_tag = tag.format(version=image_version)
    model_image = f"{image}:{model_image_tag}"
    config = get_config(config_filename)
    run_dir = get_run_dir(model_image_tag, config)
    run_model(config, run_dir, model_image, image_runner)
    run_name = config['experiment_name']
    run_reference_dir = os.path.join(reference_dir, run_name)
    md5sum_filename = os.path.join(run_reference_dir, MD5SUM_FILENAME)
    check_rundir_md5sum(run_dir, md5sum_filename)
    if 'serialize' in model_image:
        serialize_md5sum_filename = os.path.join(
            run_reference_dir, SERIALIZE_MD5SUM_FILENAME
        )
        check_rundir_md5sum(run_dir, serialize_md5sum_filename)
    shutil.rmtree(run_dir)


def test_run_emulation_train(image, image_version, monkeypatch):

    config = get_config("emulation-train.yml")
    model_image_tag = "{version}-emulation".format(version=image_version)
    model_image = f"{image}:{model_image_tag}"
    run_dir = get_run_dir(model_image_tag, config)

    monkeypatch.setenv("OUTPUT_FREQ_SEC", str(900*2))
    env_vars = ["--env", "OUTPUT_FREQ_SEC"]
    run_model(config, run_dir, model_image, "docker", additional_env_vars=env_vars)
    nc_files = os.listdir(os.path.join(run_dir, "netcdf_output"))
    assert os.path.exists(os.path.join(run_dir, "state_output.zarr"))
    assert len(nc_files) > 0
    subprocess.check_call(["sudo", "rm", "-r", run_dir])


def test_run_emulate_zc_micro(image, image_version, monkeypatch, code_root):

    config = get_config("emulation-emulate-micro.yml")
    model_image_tag = "{version}-emulation".format(version=image_version)
    model_image = f"{image}:{model_image_tag}"
    run_dir = get_run_dir(model_image_tag, config)

    monkeypatch.setenv("OUTPUT_FREQ_SEC", str(900*2))
    model_path = join(code_root, "emulation/test_model/dummy_model.tf")
    monkeypatch.setenv("TF_MODEL_PATH", model_path)
    monkeypatch.setenv("STORE_EMU_DATA", str(True))
    env_vars = [
        "--env", "OUTPUT_FREQ_SEC",
        "--env", "TF_MODEL_PATH",
        "--env", "STORE_EMU_DATA",
    ]
    run_model(config, run_dir, model_image, "docker", additional_env_vars=env_vars)
    assert os.path.exists(os.path.join(run_dir, "state_output.zarr"))
    subprocess.check_call(["sudo", "rm", "-r", run_dir])


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
        archive_mount = ['-v', f'{archive}:{archive}']
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
    docker_run = ['docker', 'run', '--rm']
    rundir_abs = os.path.abspath(rundir)
    rundir_mount = ['-v', f'{rundir_abs}:' + docker_runpath + '/rundir']
    data_abs = os.path.abspath(os.path.join(rundir_abs, 'test_data'))
    os.makedirs(data_abs, exist_ok=True)
    data_mount = ['-v', f'{data_abs}:' + docker_runpath + '/rundir/test_data']
    fv3out_filename = join(rundir, 'stdout.log')
    fv3err_filename = join(rundir, 'stderr.log')
    call = (
        docker_run + 
        rundir_mount + 
        archive_mount + 
        data_mount + 
        secret_mount + 
        env_vars + 
        [model_image] + 
        ["bash", "/rundir/submit_job.sh", str(n_processes)]
    )
    with open(fv3out_filename, 'w') as fv3out_f, open(fv3err_filename, 'w') as fv3err_f:
        subprocess.check_call(
            call,
            stdout=fv3out_f,
            stderr=fv3err_f,
        )


def run_model_sarus(rundir, model_image, n_processes):
    shutil.copy(os.path.join(TEST_DIR, "run_files/job_jenkins_sarus"), os.path.join(rundir, "job_jenkins_sarus"))
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
    shutil.copy(SUBMIT_JOB_FILENAME, os.path.join(dirname, 'submit_job.sh'))


def run_model(config, run_dir, model_image, image_runner, additional_env_vars=None):
    if os.path.isdir(run_dir):
        shutil.rmtree(run_dir)
    os.makedirs(run_dir)
    write_run_directory(config, run_dir)
    n_processes = get_n_processes(config)
    if image_runner == "docker":
        run_model_docker(run_dir, model_image, n_processes, additional_env_vars=additional_env_vars)
    elif image_runner == "sarus":
        run_model_sarus(run_dir, model_image, n_processes)
    else:
        raise NotImplementedError("image_runner must be one of 'docker' or 'sarus'")


@pytest.mark.parametrize(
    ("config_filename", "tag", "layout"), 
    [
        ("model-level-coarse-graining.yml", "{version}", [1, 2]),
        ("pressure-level-coarse-graining.yml", "{version}", [1, 2])
    ],
    ids=lambda x: str(x)
)
def test_run_reproduces_across_layouts(
    config_filename,
    tag,
    layout,
    image,
    image_version,
    image_runner,
    reference_dir
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


if __name__ == '__main__':
    pytest.main()
