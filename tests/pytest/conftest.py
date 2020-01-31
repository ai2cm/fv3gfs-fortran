
def pytest_addoption(parser):
    parser.addoption(
        "--refdir", action="store", default="reference/docker", help="directory for reference files"
    )
    parser.addoption(
        "--image_tag", action="store", default="default", help="The docker image tag to run"
    )
    parser.addoption(
        "--code_root", action="store", default="/", help="The docker image tag to run"
    )
