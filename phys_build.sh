#!/bin/bash

export CUDA=n
export DOCKER_BUILDKIT=1
export BUILD_FROM_INTERMEDIATE=y
export COMPILED_TAG_NAME=physics

make pull_deps
make build_serialize

