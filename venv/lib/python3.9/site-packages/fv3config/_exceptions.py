class InvalidFileError(FileNotFoundError):
    """Raised when a specified file is invalid, either non-existent or not as expected."""

    pass


class ConfigError(ValueError):
    pass


class DelayedImportError(object):
    """Mock module object which raises an ImportError or other user-specified error only
    once the module's attributes are accesssed.

    Can be used to raise ImportError only if user attempts to use optional dependencies.
    """

    def __init__(self, err):
        self.err = err

    def __getattr__(self, name):
        raise self.err
