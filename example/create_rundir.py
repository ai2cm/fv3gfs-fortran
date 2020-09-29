#!/usr/bin/env python3

import fv3config
import yaml
config = yaml.safe_load(open('example/config.yml', 'r'))
fv3config.write_run_directory(config, 'rundir')
