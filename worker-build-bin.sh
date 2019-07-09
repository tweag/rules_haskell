#!/bin/bash
bazel clean && bazel build --incompatible_depset_union=false --strategy=HaskellBuildBinary=worker --worker_sandboxing //tests/binary-simple
