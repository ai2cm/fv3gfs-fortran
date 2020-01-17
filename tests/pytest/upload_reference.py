import argparse
import os
import fsspec
from test_regression import REFERENCE_DIR


def parse_args():
    parser = argparse.ArgumentParser(description="Upload reference files for regression tests.")
    parser.add_argument('location', type=str, help="location for reference files")
    return parser.parse_args()


def get_fs(path: str) -> fsspec.AbstractFileSystem:
    """Return the fsspec filesystem required to handle a given path."""
    protocol, path = path.split("://")
    return fsspec.filesystem(protocol)


def get_latest_version(repository_location):
    """
    Given a location of a reference repository, give the integer of the latest version.
    
    If none exists, return 0.
    """
    fs = get_fs(repository_location)
    subdirs = [
        os.path.basename(path.strip('/'))
        for path in fs.ls(repository_location.strip('/'))
    ]
    latest_version = 0
    for subdir in subdirs:
        if subdir[:1] == 'v' and subdir[1:].isdigit():
            latest_version = max(latest_version, int(subdir[1:]))
    return latest_version


def get_reference_location(repository_location, reference_version):
    return f"{repository_location.strip('/')}/v{reference_version}"


def upload_reference(repository_location):
    fs = get_fs(repository_location)
    fs.put(REFERENCE_DIR, repository_location, recursive=True)


if __name__ == '__main__':
    args = parse_args()
    new_version = get_latest_version(args.location) + 1
    target_location = get_reference_location(args.location, new_version)
    upload_reference(target_location)
    print(f'Uploaded reference to {target_location}')
