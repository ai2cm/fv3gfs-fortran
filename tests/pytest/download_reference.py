import argparse
from upload_reference import get_latest_version, get_fs, get_reference_location
from test_regression import REFERENCE_DIR


def parse_args():
    parser = argparse.ArgumentParser(description="Download reference files for regression tests.")
    parser.add_argument('location', type=str, help="location of reference files")
    return parser.parse_args()


def download_reference(repository_location):
    fs = get_fs(repository_location)
    fs.get(repository_location, REFERENCE_DIR, recursive=True)


if __name__ == '__main__':
    args = parse_args()
    latest_version = get_latest_version(args.location)
    target_location = get_reference_location(args.location, latest_version)
    download_reference(target_location)
    print(f'Retrieved reference from {target_location}')
