import os

import yaml

from ._datastore import (
    get_initial_conditions_directory,
    get_orographic_forcing_directory,
    get_base_forcing_directory,
)
from fv3config._datastore import (
    get_field_table_filename,
    get_diag_table_filename,
    get_data_table_filename,
)
from ._exceptions import ConfigError
from . import filesystem


FV3CONFIG_YML_NAME = "fv3config.yml"


def is_dict_or_list(option):
    return isinstance(option, dict) or isinstance(option, list)


def ensure_is_list(asset):
    """If argument is a dict, return length 1 list containing dict"""
    if isinstance(asset, dict):
        return [asset]
    elif isinstance(asset, list):
        return asset
    else:
        raise ConfigError("Asset must be a dict or list of dicts")


def get_orographic_forcing_asset_list(config):
    """Return asset_list for orographic forcing"""
    source_directory = get_orographic_forcing_directory(config)
    return asset_list_from_path(
        source_directory, target_location="INPUT", copy_method="link"
    )


def get_base_forcing_asset_list(config):
    """Return asset_list for base forcing"""
    if is_dict_or_list(config["forcing"]):
        return ensure_is_list(config["forcing"])
    else:
        source_directory = get_base_forcing_directory(config)
        return asset_list_from_path(source_directory, copy_method="link")


def get_initial_conditions_asset_list(config):
    """Return asset_list for initial conditions. """
    if is_dict_or_list(config["initial_conditions"]):
        return ensure_is_list(config["initial_conditions"])
    else:
        source_directory = get_initial_conditions_directory(config)
        return asset_list_from_path(source_directory, target_location="INPUT")


def get_data_table_asset(config):
    """Return asset for data_table"""
    data_table_filename = get_data_table_filename(config)
    location, name = os.path.split(data_table_filename)
    return get_asset_dict(location, name, target_name="data_table")


def get_diag_table_asset(config):
    """Return asset for diag_table"""
    diag_table_filename = get_diag_table_filename(config)
    location, name = os.path.split(diag_table_filename)
    return get_asset_dict(location, name, target_name="diag_table")


def get_field_table_asset(config):
    """Return asset for field_table"""
    field_table_filename = get_field_table_filename(config)
    location, name = os.path.split(field_table_filename)
    return get_asset_dict(location, name, target_name="field_table")


def get_fv3config_yaml_asset(config):
    """An asset containing this configuration"""
    return get_bytes_asset_dict(
        bytes(yaml.safe_dump(config), "UTF-8"),
        target_location=".",
        target_name=FV3CONFIG_YML_NAME,
    )


def get_asset_dict(
    source_location,
    source_name,
    target_location="",
    target_name=None,
    copy_method="copy",
):
    """Helper function to generate asset for a particular file

    Args:
        source_location (str): path to directory containing source file
        source_name (str): filename of source file
        target_location (str, optional): sub-directory to which file will
            be written, relative to run directory root. Defaults to empty
            string (i.e. root of run directory).
        target_name (str, optional): filename to which file will be written.
            Defaults to None, in which case source_name is used.
        copy_method (str, optional): flag to determine whether file is
            copied ('copy') or hard-linked ('link'). Defaults to 'copy'.

    Returns:
        dict: an asset dictionary
    """
    if target_name is None:
        target_name = source_name
    asset = {
        "source_location": source_location,
        "source_name": source_name,
        "target_location": target_location,
        "target_name": target_name,
        "copy_method": copy_method,
    }
    return asset


def get_bytes_asset_dict(
    data: bytes, target_location: str, target_name: str,
):
    """Helper function to define the necessary fields for a binary asset to
    be saved at a given location.

    Args:
        data: the bytes to save
        target_location: sub-directory to which file will
            be written, relative to run directory root. Defaults to empty
            string (i.e. root of run directory).
        target_name: filename to which file will be written.
            Defaults to None, in which case source_name is used.

    Returns:
        dict: an asset dictionary
    """

    return {
        "bytes": data,
        "target_location": target_location,
        "target_name": target_name,
    }


def _without_dot(path):
    if path == ".":
        return ""
    else:
        return path


def asset_list_from_path(from_location, target_location="", copy_method="copy"):
    """Return asset_list from all files within a given path.

    Args:
        location (str): local path or google cloud storage url.
        target_location (str, optional): target_location used for generated assets.
            Defaults to '' which is root of run-directory.
        copy_method ('copy' or 'link', optional): whether to copy or link assets,
            defaults to 'copy'. If location is a google cloud storage url, this option
            is ignored and files are copied.

    Returns:
        list: a list of asset dictionaries
        """
    if not filesystem.is_local_path(from_location):
        copy_method = "copy"
    asset_list = []
    for dirname, basename, relative_target_location in _asset_walk(from_location):
        asset_list.append(
            get_asset_dict(
                dirname,
                basename,
                target_location=os.path.join(target_location, relative_target_location),
                copy_method=copy_method,
            )
        )
    return asset_list


def _asset_walk(location):
    fs = filesystem.get_fs(location)
    protocol_prefix = filesystem._get_protocol_prefix(location)
    path_list = fs.walk(location)
    for dirname, _, files in path_list:
        dirname = protocol_prefix + dirname
        subdir_target_location = os.path.relpath(dirname, start=location)
        for basename in files:
            yield dirname, basename, _without_dot(subdir_target_location)


def write_asset(asset, target_directory):
    """Write file represented by asset to target_directory

    Args:
        asset (dict): an asset dict
        target_directory (str): path to a directory in which all files will be written
    """

    target_path = os.path.join(
        target_directory, asset["target_location"], asset["target_name"]
    )

    os.makedirs(os.path.dirname(target_path), exist_ok=True)

    if "copy_method" in asset:
        copy_file_asset(asset, target_path)
    elif "bytes" in asset:
        with open(target_path, "wb") as f:
            f.write(asset["bytes"])
    else:
        raise ConfigError(
            "Cannot write asset. Asset must have either a `copy_method` or `bytes` key."
        )


def copy_file_asset(asset, target_path):
    check_asset_has_required_keys(asset)
    source_path = os.path.join(asset["source_location"], asset["source_name"])
    copy_method = asset["copy_method"]
    if copy_method == "copy":
        filesystem.get_file(source_path, target_path)
    elif copy_method == "link":
        link_file(source_path, target_path)
    else:
        raise ConfigError(
            f"Behavior of copy_method {copy_method} not defined for {source_path} asset"
        )


def write_assets_to_directory(config, target_directory):
    asset_list = config_to_asset_list(config)
    write_asset_list(asset_list, target_directory)


def write_asset_list(asset_list, target_directory):
    """Loop over list of assets and write them all"""
    for asset in asset_list:
        write_asset(asset, target_directory)


def check_asset_has_required_keys(asset):
    """Check asset has all of its required keys"""
    required_asset_keys = [
        "source_location",
        "source_name",
        "target_location",
        "target_name",
        "copy_method",
    ]
    for required_asset_key in required_asset_keys:
        if required_asset_key not in asset:
            raise ConfigError(f"Assets must have a {required_asset_key}")


def config_to_asset_list(config):
    """Convert a configuration dictionary to an asset list. The asset list
    will contain all files for the run directory except the namelist."""
    asset_list = get_initial_conditions_asset_list(config)
    asset_list += get_base_forcing_asset_list(config)
    asset_list += get_orographic_forcing_asset_list(config)
    asset_list.append(get_field_table_asset(config))
    asset_list.append(get_diag_table_asset(config))
    asset_list.append(get_data_table_asset(config))
    asset_list.append(get_fv3config_yaml_asset(config))
    if "patch_files" in config:
        if is_dict_or_list(config["patch_files"]):
            asset_list += ensure_is_list(config["patch_files"])
        else:
            raise ConfigError(
                "patch_files item in config dictionary must be an asset dict or "
                "list of asset dicts"
            )
    return asset_list


def link_file(source_item, target_item):
    if any(not filesystem.is_local_path(item) for item in [source_item, target_item]):
        raise NotImplementedError(
            "cannot perform linking operation involving remote urls "
            f"from {source_item} to {target_item}"
        )
    if os.path.exists(target_item):
        os.remove(target_item)
    os.symlink(source_item, target_item)
