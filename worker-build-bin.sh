#!/bin/bash
bazel clean && bazel build --incompatible_depset_union=false --strategy=HaskellBuildBinary=worker --strategy=HaskellLinkBinary=worker --worker_sandboxing //tests/binary-simple
