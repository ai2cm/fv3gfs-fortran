import subprocess
import os
import fsspec
import yaml
from .. import filesystem
from ._native import CONFIG_OUT_FILENAME, run_native

DOCKER_OUTDIR = "/outdir"
DOCKER_CONFIG_LOCATION = os.path.join("/", CONFIG_OUT_FILENAME)
DOCKER_RUNFILE = "/runfile.py"
DOCKER_COMMAND = ["docker", "run"]
DOCKER_KEYFILE = "/gcs_key.json"
FV3RUN_MODULE = "fv3config.fv3run"


def run_docker(
    config_dict_or_location,
    outdir,
    docker_image,
    runfile=None,
    keyfile=None,
    capture_output=True,
):
    """Run the FV3GFS model in a docker container with the given configuration.

    Copies the resulting directory to a target location. Will use the Google cloud
    storage key at ``$GOOGLE_APPLICATION_CREDENTIALS`` by default. Requires the
    fv3gfs-python package and fv3config to be installed in the docker image.

    Args:
        config_dict_or_location (dict or str): a configuration dictionary, or a
            location (local or on Google cloud storage) of a yaml file containing
            a configuration dictionary
        outdir (str): location to copy the resulting run directory
        runfile (str, optional): Python model script to use in place of the default.
        docker_image (str, optional): If given, run this command inside a container
            using this docker image. Image must have this package and fv3gfs-python
            installed.
        keyfile (str, optional): location of a Google cloud storage key to use
            inside the docker container
        capture_output (bool, optional): If true, then the stderr and stdout
            streams will be redirected to the files `outdir/stderr.log` and `outdir/stdout.log`
            respectively.
    """
    if keyfile is None:
        keyfile = os.environ.get("GOOGLE_APPLICATION_CREDENTIALS", None)

    if isinstance(config_dict_or_location, str):
        config_dict = _load_yaml(config_dict_or_location)
    else:
        config_dict = config_dict_or_location

    filesystem.get_fs(outdir).makedirs(outdir, exist_ok=True)
    bind_mount_args = []
    docker_args = []
    _get_credentials_args(keyfile, docker_args, bind_mount_args)
    _get_local_data_bind_mounts(config_dict, bind_mount_args)

    _get_docker_args(docker_args, bind_mount_args, outdir)
    runfile_in_docker = _get_runfile_args(runfile, bind_mount_args)
    if filesystem.is_local_path(outdir):
        docker_outdir = DOCKER_OUTDIR
    else:
        docker_outdir = outdir
    python_command = run_native.command(
        config_dict,
        docker_outdir,
        runfile=runfile_in_docker,
        capture_output=capture_output,
    )

    subprocess.check_call(
        DOCKER_COMMAND + bind_mount_args + docker_args + [docker_image] + python_command
    )


def _load_yaml(url):
    with fsspec.open(url) as f:
        return yaml.safe_load(f.read())


def _get_runfile_args(runfile, bind_mount_args) -> str:
    if runfile is not None:
        if filesystem.is_local_path(runfile):
            bind_mount_args += ["-v", f"{os.path.abspath(runfile)}:{DOCKER_RUNFILE}"]
            return DOCKER_RUNFILE
        else:
            return runfile


def _get_paths(config_dict):
    """Return a list of all paths referenced by the config dict."""
    return_list = [
        config_dict["diag_table"],
        config_dict["data_table"],
        config_dict["forcing"],
        config_dict["initial_conditions"],
    ]
    patch_files = config_dict.get("patch_files", [])
    if isinstance(patch_files, list):
        return_list.extend(patch_files)
    else:
        return_list.append(patch_files)
    return return_list


def _is_local_path(maybe_path_or_object):
    return (
        isinstance(maybe_path_or_object, str)
        and os.path.isabs(maybe_path_or_object)
        and filesystem.is_local_path(maybe_path_or_object)
    )


def _get_local_data_paths(config_dict):
    """Return a list of all local paths referenced by the config dict."""
    local_paths = []
    for potential_path in _get_paths(config_dict):
        if _is_local_path(potential_path):
            local_paths.append(potential_path)
        elif isinstance(potential_path, list):
            for asset in potential_path:
                if filesystem.is_local_path(asset["source_location"]):
                    local_paths.append(
                        os.path.join(asset["source_location"], asset["source_name"])
                    )
        elif isinstance(potential_path, dict):
            asset = potential_path
            if filesystem.is_local_path(asset["source_location"]):
                local_paths.append(
                    os.path.join(asset["source_location"], asset["source_name"])
                )
    return local_paths


def _get_local_data_bind_mounts(config_dict, bind_mount_args):
    for local_path in _get_local_data_paths(config_dict):
        bind_mount_args += ["-v", f"{local_path}:{local_path}"]


def _get_docker_args(docker_args, bind_mount_args, outdir):
    if filesystem.is_local_path(outdir):
        bind_mount_args += ["-v", f"{os.path.abspath(outdir)}:{DOCKER_OUTDIR}"]
    docker_args += ["--rm", "--user", f"{os.getuid()}:{os.getgid()}"]


def _get_credentials_args(keyfile, docker_args, bind_mount_args):
    if keyfile is not None:
        bind_mount_args += ["-v", f"{os.path.abspath(keyfile)}:{DOCKER_KEYFILE}"]
        docker_args += ["-e", f"GOOGLE_APPLICATION_CREDENTIALS={DOCKER_KEYFILE}"]
