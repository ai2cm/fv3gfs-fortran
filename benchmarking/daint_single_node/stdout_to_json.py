#!/usr/bin/env python3

import os, re, click, glob, time, datetime, json, sys

# this dict maps the names of the timer in the JSON file to the actual
# timers reported in stdout of FV3GFS, multiple entries are accumulated
TIMER_MAPPING = {
  "total": ["Total runtime"],
  "init": ["1-Initialization", "2-Main-loop-1st-trip"],
  "main_loop": ["3-Main-loop"],
  "dyn_core": ["3.1.1.1-dyn_core"],
  "tracer_adv": ["3.1.1.2-Tracer-advection"],
  "remapping": ["3.1.1.3-Remapping"]
}

def num(s):
    try:
        return int(s)
    except ValueError:
        return float(s)


@click.command()
@click.option('--stdout_file_regex', default='slurm-*.out', help='regex pattern to identify stdout file')
@click.argument('run_directory', nargs=1)
def stdout_to_json(stdout_file_regex, run_directory):

    # find stdout file
    files = glob.glob(os.path.join(run_directory, stdout_file_regex))
    assert len(files) == 1, 'Either no or too many matches for given stdout file name regex pattern'
    stdout_file = files[0]

    # extract timer string from stdout
    with open(stdout_file, 'r+') as f:
        stdout = f.read()
    match = re.search(r'^Total runtime.*^4-Termination', stdout, re.MULTILINE | re.DOTALL)
    assert match, 'Issue extracting timings from stdout of SLURM job'
    
    # parse raw timers
    raw_timers = {}
    labels = ['hits', 'tmin', 'tmax', 'tavg', 'tstd', 'tfrac', 'grain', 'pemin', 'pemax']
    for line in match.group().splitlines():
        name = line[0:32].strip()
        values = [num(val) for val in line[32:].split()]
        raw_timers[name] = dict(zip(labels, values))

    # convert into format for plotting
    times = {}
    for json_name, fv3_names in TIMER_MAPPING.items():
        times[json_name] = {"hits": 0, "minimum": 0., "maximum": 0., "median": 0.}
        for fv3_name in fv3_names:
            times[json_name]["hits"] += raw_timers[fv3_name]["hits"]
            times[json_name]["minimum"] += raw_timers[fv3_name]["tmin"]
            times[json_name]["maximum"] += raw_timers[fv3_name]["tmax"]
            times[json_name]["median"] += raw_timers[fv3_name]["tavg"]

    # assemble meta-data
    setup = {}
    setup["experiment time"] = datetime.datetime.fromtimestamp(os.path.getmtime(stdout_file)).strftime("%d/%m/%Y %H:%M:%S")
    setup["data set"] = os.path.basename(os.path.normpath(run_directory))
    main_loop_timer = TIMER_MAPPING["main_loop"][0]
    setup["timesteps"] = raw_timers[main_loop_timer]["hits"] + 1
    setup["version"] = "fortran"
    with open(os.path.join(run_directory, "git.env"), 'r+') as f:
        git_env = f.read()
    match = re.search(r'^GIT_BRANCH = (.*)$', git_env, re.MULTILINE)
    if match:
        setup["git branch"] = match.group(1)
    match = re.search(r'^GIT_COMMIT = ([a-z0-9]+)$', git_env, re.MULTILINE)
    if match:
        setup["git hash"] = match.group(1)

    # print as JSON
    experiment = {}
    experiment["setup"] = setup
    experiment["times"] = times
    json.dump(experiment, sys.stdout, indent=4)
    print('')

if __name__ == '__main__':
    stdout_to_json()
