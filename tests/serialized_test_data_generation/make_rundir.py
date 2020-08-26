import fv3config
import yaml
import shutil
import os
import sys
import subprocess

experiment = sys.argv[1]
workdir = os.getcwd()
target_directory = workdir + '/rundir/' + experiment
os.makedirs(target_directory, exist_ok=True)
shutil.copy("submit_job.sh", target_directory)
config = yaml.safe_load(open(os.path.join(workdir + "/configs",experiment + ".yml"), "r"))
layout = config["namelist"]["fv_core_nml"]["layout"]
num_ranks = 6 * layout[0] * layout[1]
subprocess.call(["sed -i '' 's/NUM_RANKS\=6/NUM_RANKS\=" + str(num_ranks) + "/g' " + os.path.join(target_directory, "submit_job.sh")], shell=True)
subprocess.call(["git rev-parse HEAD > " + os.path.join(target_directory, "fortran_sha.txt")], shell=True)
fv3config.write_run_directory(config, target_directory)

