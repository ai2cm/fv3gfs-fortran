#!/bin/bash
rm -rf rundirectory
write_run_directory fv3config.yml rundirectory
cp validate_radiation.py  rundirectory/
cd rundirectory
mpirun -n 6 python validate_radiation.py 

