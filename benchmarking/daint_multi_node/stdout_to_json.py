#!/usr/bin/env python3

# This script parses the standard output of a FV3GFS run to extract timing
# information and convert it to the JSON format which is used by the performance
# monitoring script.

# 2021/01/22 Oliver Fuhrer, Vulcan Inc, oliverf@vulcan.com

import os, re, click, glob, time, datetime, json, sys
import yaml

# this dict maps the names of the timer in the JSON file to the actual
# timers reported in stdout of FV3GFS, multiple entries are accumulated
# (note that only timers with the same hit count can be accumulated)
TIMER_MAPPING = {
    "total": ["Total runtime"],
    "initialization": ["1-Initialization", "2-Main-loop-1st-trip"],
    "mainloop": ["3-Main-loop"],
    "FVDynamics": ["3.1.1-fv_dynamics"],
    "DynCore": ["3.1.1.1-dyn_core"],
    "TracerAdvection": ["3.1.1.2-Tracer-advection"],
    "Remapping": ["3.1.1.3-Remapping"],
}


def num(s):
    try:
        return int(s)
    except ValueError:
        return float(s)


@click.command()
@click.option(
    "--stdout_file_regex",
    default="slurm-*.out",
    help="regex pattern to identify stdout file",
)
@click.argument("run_directory", nargs=1)
def stdout_to_json(stdout_file_regex, run_directory):
    """
    This function parses the standard output of a FV3GFS run in the run_directory to extract timing information and convert it to the JSON format, which is printed to stdout. Function does not return anything.

      Parameters:
          stdout_file_regex (string): Optional regex string to identify stdout file in run directory
          run_directory     (string): String containing path to run directory

    """

    # find stdout file
    files = glob.glob(os.path.join(run_directory, stdout_file_regex))
    assert (
        len(files) == 1
    ), "Either no or too many matches for given stdout file name regex pattern"
    stdout_file = files[0]

    # extract timer string from stdout
    with open(stdout_file, "r+") as f:
        stdout = f.read()
    match = re.search(
        r"^Total runtime.*^ *MPP_STACK", stdout, re.MULTILINE | re.DOTALL
    )
    assert match, "Issue extracting timings from stdout of SLURM job"

    # parse raw timers
    raw_timers = {}
    labels = [
        "hits",
        "tmin",
        "tmax",
        "tavg",
        "tstd",
        "tfrac",
        "grain",
        "pemin",
        "pemax",
    ]
    for line in match.group().splitlines():
        name = line[0:32].strip()
        values = [num(val) for val in line[32:].split()]
        if values:
            raw_timers[name] = dict(zip(labels, values))

    # convert into format for plotting
    times = {}
    for json_name, fv3_names in TIMER_MAPPING.items():
        times[json_name] = {"hits": None, "minimum": 0.0, "maximum": 0.0, "mean": 0.0}
        for fv3_name in fv3_names:
            if times[json_name]["hits"] is None:
                times[json_name]["hits"] = raw_timers[fv3_name]["hits"]
            else:
                assert (
                    times[json_name]["hits"] == raw_timers[fv3_name]["hits"]
                ), "Can only accumulate timers with equal hit count"
            times[json_name]["minimum"] += raw_timers[fv3_name]["tmin"]
            times[json_name]["maximum"] += raw_timers[fv3_name]["tmax"]
            times[json_name]["mean"] += raw_timers[fv3_name]["tavg"]

    # assemble meta-data
    setup = {}
    setup["dirname"] = os.path.basename(run_directory)
    setup["timestamp"] = datetime.datetime.fromtimestamp(
        os.path.getmtime(stdout_file)
    ).strftime("%d/%m/%Y %H:%M:%S")
    with open(os.path.join(run_directory, "config.yml"), "r+") as f:
        config = yaml.safe_load(f)
        setup["dataset"] = config["experiment_name"]
    dynamics_timer = TIMER_MAPPING["FVDynamics"][0]
    setup["timesteps"] = raw_timers[dynamics_timer]["hits"] + 1
    setup["version"] = "fortran"
    with open(os.path.join(run_directory, "git.env"), "r+") as f:
        git_env = f.read()
    match = re.search(r"^GIT_BRANCH = (.*)$", git_env, re.MULTILINE)
    if match:
        setup["git branch"] = match.group(1)
    match = re.search(r"^GIT_COMMIT = ([a-z0-9]+)$", git_env, re.MULTILINE)
    if match:
        setup["git hash"] = match.group(1)

    # print as JSON
    experiment = {}
    experiment["setup"] = setup
    experiment["times"] = times
    json.dump(experiment, sys.stdout, indent=4)


if __name__ == "__main__":
    stdout_to_json()
