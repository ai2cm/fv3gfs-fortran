#!/bin/bash
CONFIGS=configs/c*6*.yml
make compile_fortran
for config in $CONFIGS
do
  config_file=`basename $config .yml`
  echo "Generating data for $config_file ..."
  EXPERIMENT=$config_file make generate_and_push_data
done
