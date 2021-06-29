import sys
import logging
import contextlib
import resource
import functools
import subprocess
import inspect
import multiprocessing
import os
import tempfile
import warnings
import yaml
import json
from ..config import write_run_directory, get_n_processes, config_to_yaml
from .. import filesystem

STDOUT_FILENAME = "stdout.log"
STDERR_FILENAME = "stderr.log"
CONFIG_OUT_FILENAME = "fv3config.yml"
MPI_FLAGS = [
    "--allow-run-as-root",
    "--use-hwthread-cpus",
    "--mca",
    "btl_vader_single_copy_mechanism",
    "none",
]
RUNFILE_ENV_VAR = "FV3CONFIG_DEFAULT_RUNFILE"

logger = logging.getLogger("fv3run")


def call_via_subprocess(module):
    def decorator(func):
        signature = inspect.signature(func)

        def main(argv):
            args, kwargs = json.loads(argv[1])
            func(*args, **kwargs)

        @functools.wraps(func)
        def command(*args, **kwargs) -> str:
            # check that args and kwargs match func
            # raises TypeError if not
            signature.bind(*args, **kwargs)

            serialized = json.dumps([args, kwargs])
            return ["python", "-m", module, serialized]

        func.main = main
        func.command = command
        return func

    return decorator


@call_via_subprocess("fv3config.fv3run._native_main")
def run_native(
    config_dict_or_location, outdir, runfile=None, capture_output: bool = True
):
    """Run the FV3GFS model with the given configuration.

    Copies the resulting directory to a target location. Will use the Google cloud
    storage key at ``$GOOGLE_APPLICATION_CREDENTIALS`` by default. Requires the
    fv3gfs-python package.

    Args:
        config_dict_or_location (dict or str): a configuration dictionary, or a
            location (local or on Google cloud storage) of a yaml file containing
            a configuration dictionary
        outdir (str): location to copy the resulting run directory
        runfile (str, optional): Python model script to use in place of the default.
        capture_output (bool, optional): If true, then the stderr and stdout
            streams will be redirected to the files `outdir/stderr.log` and `outdir/stdout.log`
            respectively.
    """
    _set_stacksize_unlimited()
    with _temporary_directory(outdir) as localdir:
        config_out_filename = os.path.join(localdir, CONFIG_OUT_FILENAME)
        # we need to write the dict to the run directory for archival and also load
        # the dict, it ends up being convenient to do both at once
        config_dict = _get_config_dict_and_write(
            config_dict_or_location, config_out_filename
        )
        write_run_directory(config_dict, localdir)
        if runfile is not None:
            filesystem.get_file(
                runfile, os.path.join(localdir, os.path.basename(runfile))
            )
        with _output_stream_context(localdir, capture_output) as (stdout, stderr):
            n_processes = get_n_processes(config_dict)
            _run_experiment(
                localdir,
                n_processes,
                runfile=runfile,
                mpi_flags=_add_oversubscribe_if_necessary(MPI_FLAGS, n_processes),
                stdout=stdout,
                stderr=stderr,
            )


def _set_stacksize_unlimited():
    try:
        resource.setrlimit(
            resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY)
        )
    except ValueError:
        warnings.warn(
            "could not remove stacksize limit, may run out of memory as a result"
        )


def _add_oversubscribe_if_necessary(mpi_flags, n_processes):
    try:
        cpu_count = multiprocessing.cpu_count()
        if cpu_count < n_processes:
            mpi_flags += ["--oversubscribe"]
    except NotImplementedError:
        warnings.warn(
            "could not determine cpu count, assuming number of processors"
            "is at least as many as number of MPI tasks"
        )
    return mpi_flags


@contextlib.contextmanager
def _temporary_directory(outdir):
    fs = filesystem.get_fs(outdir)
    if not filesystem.is_local_path(outdir):
        with tempfile.TemporaryDirectory() as tempdir:
            try:
                yield tempdir
            finally:
                logger.info("Copying output to %s", outdir)
                fs.makedirs(outdir, exist_ok=True)
                filesystem.put_directory(tempdir, outdir)
    else:
        fs.makedirs(outdir, exist_ok=True)
        yield outdir


def _captured_output_context(localdir):
    out_filename = os.path.join(localdir, STDOUT_FILENAME)
    err_filename = os.path.join(localdir, STDERR_FILENAME)
    with open(out_filename, "wb") as out_file, open(err_filename, "wb") as err_file:
        try:
            yield out_file, err_file
        except subprocess.CalledProcessError as e:
            logger.critical(
                "Experiment failed. " "Check %s and %s for logs.",
                STDOUT_FILENAME,
                STDERR_FILENAME,
            )
            raise e


def _uncaptured_output_context(localdir):
    try:
        yield sys.stdout, sys.stderr
    except subprocess.CalledProcessError as e:
        logger.critical("Experiment failed")
        raise e


@contextlib.contextmanager
def _output_stream_context(localdir: str, capture_output: bool):
    logger.info("running experiment")
    if capture_output:
        yield from _captured_output_context(localdir)
    else:
        yield from _uncaptured_output_context(localdir)


def _get_python_command(runfile):
    python_args = ["python3", "-m", "mpi4py"]
    if runfile is not None:
        python_args.append(os.path.basename(runfile))
    elif RUNFILE_ENV_VAR in os.environ:
        python_args.append(os.environ[RUNFILE_ENV_VAR])
    else:
        python_args += ["-m", "fv3gfs.run"]
    return python_args


def _run_experiment(
    dirname, n_processes, runfile, mpi_flags=None, stdout=None, stderr=None
):
    if mpi_flags is None:
        mpi_flags = []

    python_command = _get_python_command(runfile)
    logger.info("Running experiment in %s", dirname)
    subprocess.check_call(
        ["mpirun", "-n", str(n_processes)] + mpi_flags + python_command,
        cwd=dirname,
        stdout=stderr,
        stderr=stdout,
    )


def _get_config_dict_and_write(config_dict_or_location, config_out_filename):
    if isinstance(config_dict_or_location, dict):
        config_dict = config_dict_or_location
        config_to_yaml(config_dict, config_out_filename)
    else:
        config_dict = _copy_and_load_config_dict(
            config_dict_or_location, config_out_filename
        )
    return config_dict


def _copy_and_load_config_dict(config_location, local_target_location):
    filesystem.get_file(config_location, local_target_location)
    with open(local_target_location, "r") as infile:
        config_dict = yaml.load(infile.read(), Loader=yaml.SafeLoader)
    return config_dict


if __name__ == "__main__":
    # In theory this warning should never be triggered.
    # There's probably a bug in run_native.command if it is.
    # Remove this main block after some time if it never gets triggered.
    warnings.warn(
        "calling fv3config.fv3run._native is deprecated, call fv3config.fv3run._native_main instead",
        DeprecationWarning,
    )
    run_native.main(sys.argv)
