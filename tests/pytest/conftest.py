DEFAULT_MODEL_IMAGE = 'us.gcr.io/vcm-ml/fv3gfs-compiled'


def pytest_addoption(parser):
    parser.addoption(
        "--refdir", action="store", default="reference/docker", help="directory for reference files"
    )
    parser.addoption(
        "--image_version", action="store", default="latest", help="The docker image version to run"
    )
    parser.addoption(
        "--image", action="store", default=DEFAULT_MODEL_IMAGE, help="The docker image name to run, without tags"
    )
    parser.addoption(
        "--code_root", action="store", default="/", help="The path to the codebase to test"
    )
