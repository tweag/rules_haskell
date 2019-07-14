#!/bin/bash
bazel clean && bazel build --strategy=HaskellBuildBinary=worker --strategy=HaskellLinkBinary=worker --worker_sandboxing //tests/binary-simple
