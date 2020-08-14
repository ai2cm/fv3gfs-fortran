#!/usr/bin/bash

export df="Dockerfile.intel2021_mpich314_nocuda"

if [[ "$df" == *"intel"* ]]; then
  echo "use srun --mpi=pmi2"
else
  echo "use srun sarus --mpi"
fi
