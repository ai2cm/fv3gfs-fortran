#!/bin/bash

cd ./rundir; ./clean.sh; cd -
docker run -it --rm --mount type=bind,source=`pwd`/rundir,target=/rundir us.gcr.io/vcm-ml/fv3gfs-compiled:physics-andrep-serialize bash

