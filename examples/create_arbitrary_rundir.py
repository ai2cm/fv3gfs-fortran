import fv3config
import yaml
import argparse
import os


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Create a run directory for FV3.")
    parser.add_argument("configfile", type=str, help="yaml configuration path")
    parser.add_argument("outdir", type=str, help="output directory")
    args = parser.parse_args()

    with open(args.configfile, "r") as f:
        config = yaml.safe_load(f)
    fv3config.write_run_directory(config, args.outdir)

    with open(os.path.join(args.outdir, "fv3config.yml"), "w") as f:
        yaml.safe_dump(config, f)
