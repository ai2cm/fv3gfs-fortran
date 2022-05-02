fv3gfs-wrapper (`import fv3gfs.wrapper`), is a Python wrapper for the FV3GFS
global climate model.

See the [documentation](https://vulcanclimatemodeling.github.io/fv3gfs-wrapper/index.html)
for more detailed instructions.

This is currently alpha development software used for research. If you would like to contribute to or collaborate on the software, please get in touch with jeremym@allenai.org or another developer.

Running the tests as they are currently written requires private credentials to google cloud services, as the data access is set to "requester pays". To run the tests and examples with your credentials, set an environment variable `GOOGLE_APPLICATION_CREDENTIALS` as a path to your JSON key file.  (E.g., `export GOOGLE_APPLICATION_CREDENTIALS=/path/to/key.json`) **Note**: your project will incur some data transfer charges if you run these tests or examples.

Building Docs
-------------

Once the docker image is built, the documentation can be built and shown using:

    make docs-docker

This will produce html documentation in `docs/html`.


Usage
-----

Example run scripts are included in [`examples/runfiles`](https://github.com/VulcanClimateModeling/fv3gfs/tree/master/sorc/fv3gfs.fd/cython_wrapper/examples/runfiles).
These run scripts act as a drop-in replacement for `fv3.exe`, and get executed
in the same way, using `mpirun`:

    mpirun -np 6 python online_code.py

Running these files requires them to be placed inside a valid run directory. This is
done automatically if you run them using `fv3run`, as is done in
the Makefile in that directory.
