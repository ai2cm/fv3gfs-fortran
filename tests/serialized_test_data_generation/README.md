To generate test data for a single experiment run

`EXPERIMENT=<experiment> make generate_test_data`

where experiment is the name of the config yaml in the configs directory (withouth the .yml extension)
To sync this with the cloud storage bucket that the fv3core regression tests you can use
`EXPERIMENT=<experiment> make pack_test_data `
`EXPERIMENT=<experiment> make push_test_data `
To tar the .dat files, and push this to the cloud bucket. If the bucket already exists the push will fail, as in normal operation we should increment the FORTRAN_VERSION number rather than overwrite data. But if it's a mistake being corrected and nobody is using it, the gsutil command is printed for you to optionally run manually if needed. 

To compile the fortran once and generate all serialized datasets in the configs directory, run
./make_all_datasets.sh
This will take awhile.
---------------------------
Coverage:
`make generate_coverage`
This uses the same mechanisms for creating a run directory and running the model, but instead uses the docker image with coverage enabled, runs it, and creates a coverage directory locally for viewing. There is no serialization when running coverage.
