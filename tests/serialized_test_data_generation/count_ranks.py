#!/bin/python
import yaml
import sys
config_file = sys.argv[1]
config = yaml.safe_load(open(config_file, "r"))
layout = config["namelist"]["fv_core_nml"]["layout"]
num_ranks = 6 * layout[0] * layout[1]
print(num_ranks)
