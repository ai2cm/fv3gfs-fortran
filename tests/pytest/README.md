# Tests

The tests use [pytest-regtest](https://pypi.org/project/pytest-regtest/) to
store regression information. To update the regression information (e.g.
checksums, ncdump outputs) simply add the `--regtest-reset` flag to the pytest
command invoking the failing test. For example::

    pytest --regtest-reset tests/pytest
    
Then, add any modified/created `.out` files to git.
