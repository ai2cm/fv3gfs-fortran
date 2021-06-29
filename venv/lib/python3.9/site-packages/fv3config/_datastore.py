import os

from .caching import get_internal_cache_dir
from .data import DATA_DIR
from ._exceptions import ConfigError
from . import filesystem


DATA_TABLE_OPTIONS = {
    "default": os.path.join(DATA_DIR, "data_table/data_table_default"),
}
DIAG_TABLE_OPTIONS = {
    "default": os.path.join(DATA_DIR, "diag_table/diag_table_default"),
    "no_output": os.path.join(DATA_DIR, "diag_table/diag_table_no_output"),
    "grid_spec": os.path.join(DATA_DIR, "diag_table/diag_table_grid_spec"),
}
DEFAULT_FIELD_TABLE_DIR = os.path.join(DATA_DIR, "field_table")
FIELD_TABLE_OPTIONS = {
    "GFDLMP": "field_table_GFDLMP",
    "ZhaoCarr": "field_table_ZhaoCarr",
}


def get_resolution(config):
    """Get the model resolution based on a configuration dictionary.

    Args:
        config (dict): a configuration dictionary

    Returns:
        resolution (str): a model resolution (e.g. 'C48' or 'C96')

    Raises:
        ConfigError: if the number of processors in x and y on a tile are unequal
    """
    npx = config["namelist"]["fv_core_nml"]["npx"]
    npy = config["namelist"]["fv_core_nml"]["npy"]
    if npx != npy:
        raise ConfigError(
            f"npx and npy in fv_core_nml must be equal, but are {npx} and {npy}"
        )
    resolution = f"C{npx-1}"
    return resolution


def get_orographic_forcing_directory(config):
    """Return the string path of the orographic forcing directory
    specified by a config dictionary.
    """
    resolution = get_resolution(config)
    if "orographic_forcing" not in config:
        raise ConfigError("config dictionary must have an 'orographic_forcing' key")
    parent_dirname = config["orographic_forcing"]
    ensure_exists(parent_dirname, "orographic_forcing")
    dirname = os.path.join(parent_dirname, resolution)
    fs = filesystem.get_fs(dirname)
    if not fs.isdir(dirname):
        valid_options = fs.listdir(parent_dirname)
        raise ConfigError(
            f"resolution {resolution} orographic forcing is not present at {dirname},"
            f" valid options are {valid_options}"
        )
    return dirname


def get_base_forcing_directory(config):
    """Return the string path of the base forcing directory
    specified by a config dictionary.
    """
    if "forcing" not in config:
        raise ConfigError("config dictionary must have a 'forcing' key")
    ensure_exists(config["forcing"], "forcing")
    return config["forcing"]


def get_initial_conditions_directory(config):
    """Return the string path of the initial conditions directory
    specified by a config dictionary.
    """
    if "initial_conditions" not in config:
        raise ConfigError("config dictionary must have an 'initial_conditions' key")
    ensure_exists(config["initial_conditions"], "initial_conditions")
    return config["initial_conditions"]


def ensure_exists(location: str, location_name: str):
    if not filesystem.get_fs(location).exists(location):
        raise ConfigError(f"{location_name} location {location} does not exist")


def check_if_data_is_downloaded():
    """Removed, do not use."""
    raise NotImplementedError("check_if_data_is_downloaded has been removed")


def ensure_data_is_downloaded():
    """Removed, do not use."""
    raise NotImplementedError("ensure_data_is_downloaded has been removed")


def refresh_downloaded_data():
    """Removed, do not use."""
    raise NotImplementedError("refresh_downloaded_data has been removed")


def resolve_option(option, built_in_options_dict):
    """Determine whether a configuration dictionary option is a built-in option or
    not and return path to file or directory representing option. An option is
    assumed to be built-in if it is not an absolute path and does not begin with gs://

    Args:
        option (str): an option
        built_in_options_dict (dict): built-in options

    Returns:
        (str): a path or url

    Raises:
        ConfigError: if option is an absolute path but does not exist or if
                     option is not in default_options_dict
    """
    if filesystem.isabs(option):
        if filesystem.get_fs(option).exists(option):
            return option
        else:
            raise ConfigError(f"The provided path {option} does not exist.")
    else:
        if option in built_in_options_dict:
            return os.path.join(get_internal_cache_dir(), built_in_options_dict[option])
        else:
            raise ConfigError(
                f"The provided option {option} is not one of the built in options: "
                f"{list(built_in_options_dict.keys())}. "
                "Paths to local files or directories must be absolute."
            )


def get_microphysics_name(config):
    """Get name of microphysics scheme from configuration dictionary

    Args:
        config (dict): a configuration dictionary

    Returns:
        str: name of microphysics scheme

    Raises:
        NotImplementedError: no microphysics name defined for specified
            imp_physics and ncld combination
    """
    imp_physics = config["namelist"]["gfs_physics_nml"].get("imp_physics")
    ncld = config["namelist"]["gfs_physics_nml"].get("ncld")
    if imp_physics == 11 and ncld == 5:
        microphysics_name = "GFDLMP"
    elif imp_physics == 99 and ncld == 1:
        microphysics_name = "ZhaoCarr"
    else:
        raise NotImplementedError(
            f"Microphysics choice imp_physics={imp_physics} and ncld={ncld} not one of the valid options"
        )
    return microphysics_name


def _return_or_infer_field_table_filename(config, field_table):
    """Return or infer the field_table filename based on the config"""
    if filesystem.get_fs(field_table).isfile(field_table):
        return field_table
    elif filesystem.get_fs(field_table).isdir(field_table):
        return _infer_field_table_filename(config, field_table)
    else:
        return field_table


def get_field_table_filename(config):
    """Get field_table filename given configuration dictionary

    Args:
        config (dict): a configuration dictionary

    Returns:
        str: field_table filename

    Raises:
        ConfigError
    """
    field_table = config.get("field_table", DEFAULT_FIELD_TABLE_DIR)
    field_table_filename = _return_or_infer_field_table_filename(config, field_table)
    if not filesystem.is_existing_absolute_path(field_table_filename):
        raise ConfigError(
            f"field_table={field_table} must either be left unset or set "
            "to an existing absolute path to a file or directory"
        )
    else:
        return field_table_filename


def _infer_field_table_filename(config, field_table_directory):
    """Infer field_table filename given configuration dictionary

    The inference is made based on settings for the microphysics.

    Args:
        config (dict): a configuration dictionary
        field_table_directory (str): a directory containing the field_table

    Returns:
        str: field_table filename

    Raises:
        NotImplementedError: if field_table for microphysics option specified
            in config has not been implemented
    """
    microphysics_name = get_microphysics_name(config)
    if microphysics_name in FIELD_TABLE_OPTIONS.keys():
        filename = FIELD_TABLE_OPTIONS[microphysics_name]
    else:
        raise NotImplementedError(
            f"Field table does not exist for {microphysics_name} microphysics"
        )
    return os.path.join(field_table_directory, filename)


def get_diag_table_filename(config):
    """Return filename for diag_table specified in config

    Args:
        config (dict): a configuration dictionary

    Returns:
        str: diag_table filename
    """
    if "diag_table" not in config:
        raise ConfigError("config dictionary must have a 'diag_table' key")
    return resolve_option(config["diag_table"], DIAG_TABLE_OPTIONS)


def get_data_table_filename(config):
    """Return filename for data_table specified in config

    Args:
        config (dict): a configuration dictionary

    Returns:
        str: data_table filename
    """
    if "data_table" not in config:
        raise ConfigError("config dictionary must have a 'data_table' key")
    return resolve_option(config["data_table"], DATA_TABLE_OPTIONS)
