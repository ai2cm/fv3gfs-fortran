#!/bin/bash

set -e
set -x

filename=fv3gfs-data-docker_2020-01-13.tar.gz
url=http://storage.googleapis.com/vcm-ml-public/$filename
datadir_local=inputdata

mkdir -p $datadir_local

# download data
[[ -f $filename ]] || wget $url

# unzip/tar input data
tar xzf $filename -C $datadir_local

