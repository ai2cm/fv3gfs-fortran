
def pytest_addoption(parser):
    parser.addoption(
        "--refdir", action="store", default="reference/docker", help="directory for reference files"
    )
