#!/usr/bin/env python3

# This script parses the standard output of a FV3GFS run to extract timing
# information and convert it to the JSON format which is used by the performance
# monitoring script.

# 2021/01/22 Oliver Fuhrer, Vulcan Inc, oliverf@vulcan.com
# 2021/05/28 Tobias Wicky, Vulcan Inc, tobiasw@vulcan.com
# 2022/09/01 Tobias Wicky, AI2, tobiasw@allenai.org

import datetime
import glob
import json
import os
import re
import sys
from typing import Any, Dict

import click
import numpy as np
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


def find_output_file(directory: str, file_regex: str) -> str:
    """Finds the file with the given regex string in a directory"""
    files = glob.glob(os.path.join(directory, file_regex))
    assert (
        len(files) == 1
    ), "Either no or too many matches for given stdout file name regex pattern"
    return files[0]


def extract_times_from_file(file_name: str):
    """extracts the timing strings from a fortran output file"""
    with open(file_name, "r+") as f:
        content = f.read()
    match = re.search(
        r"^Total runtime.*^ *MPP_STACK", content, re.MULTILINE | re.DOTALL
    )
    assert match, "Issue extracting timings from stdout of SLURM job"
    return match


def parse_match_for_times(match) -> Dict[str, Dict[str, float]]:
    """converts the raw strings containing the times into a dictionary"""
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
        values = [string_to_numeric_value(val) for val in line[32:].split()]
        if values:
            raw_timers[name] = dict(zip(labels, values))
    return raw_timers


def collect_mean_times(raw_timers: Dict[str, Dict[str, float]]) -> Dict[str, Any]:
    times = {}
    for json_name, fv3_names in TIMER_MAPPING.items():
        times[json_name] = {"hits": None, "mean": 0.0}
        for fv3_name in fv3_names:
            if times[json_name]["hits"] is None:
                times[json_name]["hits"] = raw_timers[fv3_name]["hits"]
            else:
                assert (
                    times[json_name]["hits"] == raw_timers[fv3_name]["hits"]
                ), "Can only accumulate timers with equal hit count"
            times[json_name]["mean"] += raw_timers[fv3_name]["tavg"]
    return times


def mock_data_per_timestep(
    times: Dict[str, Any], total_steps: int, ranks: int
) -> Dict[str, Any]:
    for json_name in TIMER_MAPPING.keys():
        times[json_name]["times"] = []
        for rank in range(ranks):
            time_per_step = []
            for _ in range(total_steps):
                time_per_step.append(times[json_name]["mean"] / total_steps)
            times[json_name]["times"].append(time_per_step)
        del times[json_name]["mean"]
    return times


def generate_output_from_times(
    raw_timers: Dict[str, Dict[str, float]], setup: Dict[str, Any]
) -> Dict[str, Any]:
    """converts the raw data from fortran to the format used in fv3core's data collection"""
    times = collect_mean_times(raw_timers)
    total_steps = setup["timesteps"] - 1
    ranks = setup["MPI ranks"]
    times = mock_data_per_timestep(times, total_steps, ranks)
    return times


def meta_data_from_git_env(setup: Dict[str, Any], run_directory: str) -> Dict[str, Any]:
    with open(os.path.join(run_directory, "git.env"), "r+") as f:
        git_env = f.read()
    match = re.search(r"^GIT_BRANCH = (.*)$", git_env, re.MULTILINE)
    if match:
        setup["git branch"] = match.group(1)
    match = re.search(r"^GIT_COMMIT = ([a-z0-9]+)$", git_env, re.MULTILINE)
    if match:
        setup["git hash"] = match.group(1)
    return setup


def meta_data_from_output(
    setup: Dict[str, Any],
    stdout_file: str,
    raw_timers: Dict[str, Dict[str, float]],
    run_directory: str,
) -> Dict[str, Any]:
    setup["dirname"] = os.path.basename(run_directory)
    setup["timestamp"] = datetime.datetime.fromtimestamp(
        os.path.getmtime(stdout_file)
    ).strftime("%d/%m/%Y %H:%M:%S")
    setup["backend"] = "fortran"
    dynamics_timer = TIMER_MAPPING["FVDynamics"][0]
    setup["timesteps"] = raw_timers[dynamics_timer]["hits"] + 1
    return setup


def meta_data_from_config(setup: Dict[str, Any], run_directory: str) -> Dict[str, Any]:
    with open(os.path.join(run_directory, "config.yml"), "r+") as f:
        config = yaml.safe_load(f)
        setup["dataset"] = config["experiment_name"]
        ranks_string = config["experiment_name"].split("_")[1]
        ranks = ""
        for character in ranks_string:
            if character.isdigit():
                ranks = ranks + character
        setup["MPI ranks"] = int(ranks)
    return setup


def assemble_meta_data(
    stdout_file: str, run_directory: str, raw_timers: Dict[str, Dict[str, float]]
):
    """assmebles meta-data about the run"""
    setup = {}
    setup["format_version"] = 3
    setup["comment"] = "Values generated from means - no detailed info available"
    setup = meta_data_from_output(setup, stdout_file, raw_timers, run_directory)
    setup = meta_data_from_config(setup, run_directory)
    setup = meta_data_from_git_env(setup, run_directory)

    return setup


def combine_outputs(setup: Dict[str, Any], times: Dict[str, Any]):
    experiment = {}
    experiment["setup"] = setup
    experiment["times"] = times
    return experiment


def print_to_output(experiment: Dict[str, Any], output=sys.stdout):
    """Prints the collected data into the specified output"""
    json.dump(experiment, output, indent=4)


def extract_dt(experiment: Dict[str, Any], output_file: str):
    with open(output_file, "r+") as f:
        content = f.read()
        match = re.search(r"^.*dtf.*", content, re.MULTILINE)
        assert len(match.group().splitlines()) == 1
        for line in match.group().splitlines():
            time = string_to_numeric_value(line[21:])
    experiment["dt_atmos"] = time
    return experiment


def calculate_sypd(experiment: Dict[str, Any]) -> float:

    if "mainloop" in experiment["times"]:
        mainloop = np.mean(sum(experiment["times"]["mainloop"].times, []))
        speedup = experiment["dt_atmos"] / mainloop
        sypd = 1.0 / 365.0 * speedup
    else:
        sypd = -999.0
    experiment["SYPD"] = sypd
    return experiment


def string_to_numeric_value(s):
    """Conversion of a number into int if possible, float otherwise"""
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
    output_file = find_output_file(run_directory, stdout_file_regex)
    match = extract_times_from_file(output_file)
    raw_timers = parse_match_for_times(match)
    setup = assemble_meta_data(output_file, run_directory, raw_timers)
    times = generate_output_from_times(raw_timers, setup)
    experiment = combine_outputs(setup, times)
    experiment = extract_dt(experiment, output_file)
    experiment = calculate_sypd(experiment)
    print_to_output(experiment)


if __name__ == "__main__":
    stdout_to_json()
