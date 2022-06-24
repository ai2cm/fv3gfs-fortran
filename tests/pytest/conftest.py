from pathlib import Path
import platform
import subprocess
import pytest
import fv3config

DEFAULT_MODEL_IMAGE = "us.gcr.io/vcm-ml/fv3gfs-compiled"


def pytest_addoption(parser):
    parser.addoption(
        "--refdir",
        action="store",
        default="reference/circleci",
        help="directory for reference files",
    )
    parser.addoption(
        "--image_version",
        action="store",
        default="latest",
        help="The image version to run",
    )
    parser.addoption(
        "--image_runner",
        action="store",
        default="docker",
        choices=("docker", "sarus"),
        help="The image runner to use",
    )
    parser.addoption(
        "--image",
        action="store",
        default=DEFAULT_MODEL_IMAGE,
        help="The image name to run, without tags",
    )
    parser.addoption(
        "--code_root",
        action="store",
        default="/",
        help="The path to the codebase to test",
    )
    parser.addoption(
        "--native",
        action="store_true",
        help="Run tests natively. Assumes FV3/fv3.exe exists and is executable. "
        "Skips image tests.",
    )


@pytest.fixture(params=[platform.system()])
def system_regtest(regtest):
    # A hack to get the system name into the regtest names
    # e.g. tests/pytest/test_regression.py::test_checksum_emulation[Linux]
    return regtest


@pytest.fixture(scope="session")
def run_native(request):
    root = Path(__file__).parent.parent.parent
    exe = root / "FV3" / "fv3.exe"

    if not request.config.getoption("--native"):
        pytest.skip()

    def run_native(config, run_dir: str, error_expected=False):
        fv3config.write_run_directory(config, run_dir)
        completed_process = subprocess.run(
            ["mpirun", "-n", "6", exe.absolute().as_posix()],
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

    return run_native
