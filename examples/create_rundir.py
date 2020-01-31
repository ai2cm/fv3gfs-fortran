import fv3config
import yaml
config = yaml.safe_load(open('fv3config.yml', 'r'))
fv3config.write_run_directory(config, 'rundir')
