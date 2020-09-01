To compile the fortran once and generate all serialized datasets in the configs directory, run
./make_all_datasets.sh
This will take a long time.

This will make data on your local hard-disk, make sure you have many GB of space available. Data will show up in rundir and test_data

To generate test data for a single experiment run
1. Build the fortran image:
`make compile_fortran`
2. Generate the data and push it to the cloud:
'`EXPERIMENT=<experiment> make generate_and_push_data`'
where experiment is the name of the config yaml in the configs directory (withouth the .yml extension)

Or in multiple steps:
1. Make the test data for a particular experiment
`EXPERIMENT=<experiment> make generate_test_data`

2. To sync this with the cloud storage bucket that the fv3core regression tests you pack the data (tar the .dat files):
`EXPERIMENT=<experiment> make pack_test_data `
3. Push to the cloud bucket
`EXPERIMENT=<experiment> make push_test_data `
 If the bucket already exists the push will fail, as in normal operation we should increment the FORTRAN_VERSION number rather than overwrite data. But if it's a mistake being corrected and nobody is using it, the gsutil command is printed for you to optionally run manually if needed. 


---------------------------
Coverage:
`make generate_coverage`
This uses the same mechanisms for creating a run directory and running the model, but instead uses the docker image with coverage enabled, runs it, and creates a coverage directory locally for viewing. There is no serialization when running coverage.
