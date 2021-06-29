import argparse

import yaml
import fsspec

import fv3config


def _parse_write_run_directory_args():
    parser = argparse.ArgumentParser("write_run_directory")
    parser.add_argument(
        "config", help="URI to fv3config yaml file. Supports any path used by fsspec."
    )
    parser.add_argument(
        "rundir", help="Desired output directory. Must be a local directory"
    )
    return parser.parse_args()


def _parse_enable_restart_args():
    parser = argparse.ArgumentParser("enable_restart")
    parser.add_argument(
        "config",
        help="URI to fv3config yaml file. Supports any path used by fsspec. "
        "File will be modified in place.",
    )
    parser.add_argument(
        "initial_conditions", help="Path to restart initial conditions.",
    )
    return parser.parse_args()


def _parse_update_config_for_nudging_args():
    parser = argparse.ArgumentParser("update_config_for_nudging")
    parser.add_argument(
        "config",
        help="URI to fv3config yaml file. Supports any path used by fsspec. "
        "File will be modified in place.",
    )
    return parser.parse_args()


def write_run_directory():
    args = _parse_write_run_directory_args()

    with fsspec.open(args.config) as f:
        config = yaml.safe_load(f)

    fv3config.write_run_directory(config, args.rundir)


def enable_restart():
    args = _parse_enable_restart_args()

    with fsspec.open(args.config) as f:
        config = yaml.safe_load(f)

    restart_config = fv3config.enable_restart(config, args.initial_conditions)

    with fsspec.open(args.config, mode="w") as f:
        yaml.safe_dump(restart_config, f)


def update_config_for_nudging():
    args = _parse_update_config_for_nudging_args()

    with fsspec.open(args.config) as f:
        config = yaml.safe_load(f)

    # only update config if nudging is turned on
    if config["namelist"]["fv_core_nml"].get("nudge", False):
        fv3config.update_config_for_nudging(config)

        with fsspec.open(args.config, mode="w") as f:
            yaml.safe_dump(config, f)
