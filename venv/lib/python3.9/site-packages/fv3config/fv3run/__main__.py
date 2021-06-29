import sys
import argparse
from ._docker import run_docker
from ._kubernetes import run_kubernetes
from ._native import run_native, RUNFILE_ENV_VAR
import yaml
import logging

MODULE_NAME = "fv3config.run"
STDOUT_FILENAME = "stdout.log"
STDERR_FILENAME = "stderr.log"
DOCKER_FLAGS = "-it"


def _parse_args():
    parser = argparse.ArgumentParser(
        description="""Run the FV3GFS model.

Will use google cloud storage key at $GOOGLE_APPLICATION_CREDENTIALS by default.
"""
    )
    parser.add_argument(
        "config", type=str, action="store", help="location of fv3config yaml file"
    )
    parser.add_argument(
        "outdir",
        type=str,
        action="store",
        help="location to copy final run directory, used as run directory if local",
    )
    parser.add_argument(
        "--runfile",
        type=str,
        action="store",
        help="Location of python script to execute with mpirun. If not specified, a "
        f"default is used, which can be overriden by setting the {RUNFILE_ENV_VAR}.",
    )
    parser.add_argument(
        "--dockerimage",
        type=str,
        action="store",
        help="if passed, execute inside a docker image with the given name",
    )
    parser.add_argument(
        "--keyfile",
        type=str,
        action="store",
        help="google cloud storage key to use for cloud copy commands",
    )
    parser.add_argument(
        "--kubernetes",
        action="store_true",
        default=False,
        help=(
            "if given, ignore --keyfile and output a yaml kubernetes config to stdout "
            "instead of submitting a run"
        ),
    )
    parser.add_argument(
        "--capture-output",
        action="store_true",
        default=False,
        help="If given, save the outputs of the fv3gfs call in a outdir/stderr.log and "
        "outdir/stdout.log. Not recommended for use with docker or kubernetes. "
        "It is recommended to use default linux pipes or docker's and kuberentes' logging "
        "functionality.",
    )
    return parser.parse_args()


def main():
    """Run the FV3GFS model based on a configuration dictionary.
    Copies the resulting run directory to a target location.
    """
    args = _parse_args()
    logging.basicConfig(level=logging.INFO)

    if args.dockerimage is not None:
        if args.kubernetes:
            job = run_kubernetes(
                args.config,
                args.outdir,
                args.dockerimage,
                runfile=args.runfile,
                submit=False,
            )
            yaml.dump(job.to_dict(), stream=sys.stdout)
        else:
            run_docker(
                args.config,
                args.outdir,
                args.dockerimage,
                runfile=args.runfile,
                keyfile=args.keyfile,
                capture_output=args.capture_output,
            )
    else:
        run_native(
            args.config,
            args.outdir,
            runfile=args.runfile,
            capture_output=args.capture_output,
        )


if __name__ == "__main__":
    sys.exit(main())
