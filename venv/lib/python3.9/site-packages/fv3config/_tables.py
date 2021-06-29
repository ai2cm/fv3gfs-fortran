import os
from ._exceptions import ConfigError

package_directory = os.path.dirname(os.path.realpath(__file__))


def update_diag_table_for_config(config, current_date, diag_table_filename):
    """Re-write first two lines of diag_table_filename with experiment_name
    and current_date from config dictionary.

    Args:
        config (dict): a configuration dictionary
        current_date (list): a list of 6 integers representing current_date
        diag_table_filename (str): diag_table filename
    """
    if "experiment_name" not in config:
        raise ConfigError("config dictionary must have a 'experiment_name' key")
    temporary_diag_table_filename = f"{diag_table_filename}_temporary"
    with open(diag_table_filename) as diag_table:
        lines = diag_table.read().splitlines()
        lines[0] = config["experiment_name"]
        lines[1] = " ".join([str(x) for x in current_date])
        with open(temporary_diag_table_filename, "w") as temporary_diag_table:
            temporary_diag_table.write("\n".join(lines))
    os.replace(temporary_diag_table_filename, diag_table_filename)
