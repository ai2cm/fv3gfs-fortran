Reference files exist in subdirectories of `reference`, e.g. `reference/circleci` for
the baseline md5 checksums for Circle CI. 

Tests need to be passed a reference directory to use. They can be run with
`pytest --refdir=$(pwd)/reference/circleci`. After running, the output of the
tests will be present in `output`. These output files can be used to update the
references, if non-bit-for-bit changes have occurred and those changes are valid.

These files can be updated using
`set_reference.sh`, by calling e.g. `./set_reference.sh latest-serialize $(pwd)/reference/circleci`.
This script requires you give it an image tag (e.g. `latest-serialize`) and a
reference directory. The updated references need to be committed into version control.

Considering whether a non-bit-for-bit change is valid is left to the user. If you
don't expect the model output to be changing, you should probably think hard before
updating the reference md5sum!

Test configurations are stored in `config` as fv3config yaml files. Adding new
yaml files to this directory will add new regression tests automatically.

These tests also support running on sarus using the SLURM scheduler, by setting `--image-runner=sarus`
as an argument to `pytest`.

