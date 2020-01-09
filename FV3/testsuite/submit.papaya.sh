#!/bin/bash

# be loud and exit upon error
set -x
set -e

# avoid segfaults
ulimit -s 12288

# run testsuite

source ./setup.sh

${testsuite} ${config} -n 6 --nprocio 0 -v 1 --color -f --tolerance=TOLERANCE_${real_type} --testlist=testlist.xml -o testsuite.out --mpicmd='mpirun --allow-run-as-root  --mca btl_vader_single_copy_mechanism none --oversubscribe -np' ${steps} ${only} ${tune}

source ./teardown.sh

