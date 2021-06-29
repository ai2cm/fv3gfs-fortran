import os
import re
from datetime import timedelta
from .._exceptions import ConfigError
from .default import NAMELIST_DEFAULTS
from .._asset_list import config_to_asset_list
from ..filesystem import get_fs


def get_n_processes(config):
    n_tiles = config["namelist"]["fv_core_nml"].get(
        "ntiles", NAMELIST_DEFAULTS["ntiles"]
    )
    layout = config["namelist"]["fv_core_nml"].get(
        "layout", NAMELIST_DEFAULTS["layout"]
    )
    processors_per_tile = layout[0] * layout[1]
    return n_tiles * processors_per_tile


def get_run_duration(config):
    """Return a timedelta indicating the duration of the run.

    Args:
        config (dict): a configuration dictionary

    Returns:
        duration (timedelta): the duration of the run

    Raises:
        ValueError: if the namelist contains a non-zero value for "months"
    """
    coupler_nml = config["namelist"].get("coupler_nml", {})
    months = coupler_nml.get("months", 0)
    if months != 0:  # months have no set duration and thus cannot be timedelta
        raise ValueError(f"namelist contains non-zero value {months} for months")
    return timedelta(
        **{
            name: coupler_nml.get(name, 0)
            for name in ("seconds", "minutes", "hours", "days")
        }
    )


def get_current_date(config):
    """Return current_date from configuration dictionary. This function may read from
    the remote initial_conditions path in the given configuration dictionary.

    Args:
        config (dict): a configuration dictionary

    Returns:
        list: current_date as list of ints [year, month, day, hour, min, sec]
    """
    force_date_from_namelist = config["namelist"]["coupler_nml"].get(
        "force_date_from_namelist", False
    )
    # following code replicates the logic that the fv3gfs model uses to determine the current_date
    if force_date_from_namelist:
        current_date = config["namelist"]["coupler_nml"].get(
            "current_date", [0, 0, 0, 0, 0, 0]
        )
    else:
        coupler_res_filename = _get_coupler_res_filename(config)
        if coupler_res_filename is not None:
            current_date = _get_current_date_from_coupler_res(coupler_res_filename)
        else:
            current_date = config["namelist"]["coupler_nml"].get(
                "current_date", [0, 0, 0, 0, 0, 0]
            )
    return current_date


def _get_current_date_from_coupler_res(coupler_res_filename):
    """Return current_date specified in coupler.res file

    Args:
        coupler_res_filename (str): a coupler.res filename

    Returns:
        list: current_date as list of ints [year, month, day, hour, min, sec]
    """
    fs = get_fs(coupler_res_filename)
    with fs.open(coupler_res_filename, mode="r") as f:
        third_line = f.readlines()[2]
        current_date = [int(d) for d in re.findall(r"\d+", third_line)]
        if len(current_date) != 6:
            raise ConfigError(
                f"{coupler_res_filename} does not have a valid current model time (need six integers on third line)"
            )
    return current_date


def _get_coupler_res_filename(config):
    """Return source path for coupler.res file, if it exists in config assets."""
    asset_list = config_to_asset_list(config)
    source_path = None
    for item in asset_list:
        target_path = os.path.join(item["target_location"], item["target_name"])
        if target_path == "INPUT/coupler.res":
            if "bytes" in item:
                raise NotImplementedError(
                    "Using a bytes dict to represent a coupler.res file is not "
                    "implemented yet. Use a standard asset dict for this item."
                )
            source_path = os.path.join(item["source_location"], item["source_name"])
    return source_path


def get_timestep(config):
    """Get the model timestep from a configuration dictionary.

    Args:
        config (dict): a configuration dictionary

    Returns:
        datetime.timedelta: the model timestep
    """
    return timedelta(seconds=config["namelist"]["coupler_nml"]["dt_atmos"])
