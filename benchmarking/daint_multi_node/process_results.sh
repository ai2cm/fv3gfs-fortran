#!/bin/bash

resdir=./results_`date +%y%m%d`
mkdir -p ${resdir}

for rundir in scratch/* ; do
  compgen -G "${rundir}/slurm-*.out" > /dev/null || continue
  grep -q 'MPP_STACK' ${rundir}/*.out || continue
  run=`basename ${rundir}`
  dir=${resdir}/${run}
  mkdir -p ${dir}
  cp ${rundir}/*.out ${dir}/
  cp ${rundir}/*.env ${dir}/
  cp ${rundir}/*.nml ${dir}/
  cp ${rundir}/*.yml ${dir}/
  ./stdout_to_json.py ${dir} > ${dir}/results.json
done
