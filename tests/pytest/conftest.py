from pathlib import Path
import platform
import re
import subprocess
import pytest
import fv3config


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


@pytest.fixture(scope="session", params=EXECUTABLES.keys())
def executable(request):
    return EXECUTABLES[request.param]


@pytest.fixture(params=[platform.system()])
def system_regtest(regtest):
    # A hack to get the system name into the regtest names
    # e.g. tests/pytest/test_regression.py::test_checksum_emulation[Linux]
    return regtest


@pytest.fixture(scope="session")
def run_native(request, executable):
    def run_native(config, run_dir: str, error_expected=False):
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

    return run_native
