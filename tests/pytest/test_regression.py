import os
from os.path import join
import yaml
import shutil
import subprocess
import pytest
import fv3config
from glob import glob


TEST_DIR = os.path.dirname(os.path.realpath(__file__))
REFERENCE_DIR = os.path.join(TEST_DIR, 'reference')
OUTPUT_DIR = os.path.join(TEST_DIR, 'output')
CONFIG_DIR = os.path.join(TEST_DIR, 'config')
SUBMIT_JOB_FILENAME = os.path.join(TEST_DIR, 'run_files/submit_job.sh')
STDOUT_FILENAME = 'stdout.log'
STDERR_FILENAME = 'stderr.log'
MD5SUM_FILENAME = "md5_gnu8.txt"
SERIALIZE_MD5SUM_FILENAME = "md5_serialize.txt"

USE_LOCAL_ARCHIVE = True

config_filenames = os.listdir(CONFIG_DIR)


@pytest.fixture(params=["{version}", "{version}-serialize"])
def model_image_tag(request):
    return request.param.format(version=request.config.getoption("--image_version"))


@pytest.fixture
def model_image(request, model_image_tag):
    model_image = request.config.getoption("--image")
    return model_image + ':' + model_image_tag


@pytest.fixture
def reference_dir(request):
    return request.config.getoption("--refdir")


@pytest.fixture(params=config_filenames)
def config(request):
    config_filename = os.path.join(CONFIG_DIR, request.param)
    with open(config_filename, 'r') as config_file:
        return yaml.safe_load(config_file)


@pytest.fixture
def run_dir(model_image_tag, config):
    run_name = config['experiment_name']
    return os.path.join(OUTPUT_DIR, model_image_tag, run_name)


def test_regression(config, model_image, reference_dir, run_dir):
    run_name = config['experiment_name']
    run_reference_dir = os.path.join(reference_dir, run_name)
    if os.path.isdir(run_dir):
        shutil.rmtree(run_dir)
    os.makedirs(run_dir)
    write_run_directory(config, run_dir)
    run_model(run_dir, model_image)
    md5sum_filename = os.path.join(run_reference_dir, MD5SUM_FILENAME)
    check_rundir_md5sum(run_dir, md5sum_filename)
    if 'serialize' in model_image:
        serialize_md5sum_filename = os.path.join(
            run_reference_dir, SERIALIZE_MD5SUM_FILENAME
        )
        check_rundir_md5sum(run_dir, serialize_md5sum_filename)
    shutil.rmtree(run_dir)


def check_rundir_md5sum(run_dir, md5sum_filename):
    ensure_reference_exists(md5sum_filename)
    subprocess.check_call(["md5sum", "-c", md5sum_filename], cwd=run_dir)


def ensure_reference_exists(filename):
    if not os.path.isfile(filename):
        raise AssertionError(
            f"reference md5sum does not exist at " + filename + ","
            f" you can create it with `set_reference.sh` -- refer to README.md"
        )


def run_model(rundir, model_image):
    if USE_LOCAL_ARCHIVE:
        archive = fv3config.get_cache_dir()
        archive_mount = ['-v', f'{archive}:{archive}']
    else:
        archive_mount = []
    docker_runpath = ""
    docker_run = ['docker', 'run', '--rm']
    rundir_abs = os.path.abspath(rundir)
    rundir_mount = ['-v', f'{rundir_abs}:' + docker_runpath + '/rundir']
    data_abs = os.path.abspath(os.path.join(rundir_abs, 'test_data'))
    os.makedirs(data_abs, exist_ok=True)
    data_mount = ['-v', f'{data_abs}:' + docker_runpath + '/rundir/test_data']
    fv3out_filename = join(rundir, 'stdout.log')
    fv3err_filename = join(rundir, 'stderr.log')
    with open(fv3out_filename, 'w') as fv3out_f, open(fv3err_filename, 'w') as fv3err_f:
        subprocess.check_call(
            docker_run + rundir_mount + archive_mount + data_mount + [model_image] + ["bash", "/rundir/submit_job.sh"],
            stdout=fv3out_f,
            stderr=fv3err_f
        )


def check_md5sum(run_dir, md5sum_filename):
    subprocess.check_call(["md5sum", "-c", md5sum_filename], cwd=run_dir)


def write_run_directory(config, dirname):
    fv3config.write_run_directory(config, dirname)
    shutil.copy(SUBMIT_JOB_FILENAME, os.path.join(dirname, 'submit_job.sh'))


if __name__ == '__main__':
    pytest.main()
