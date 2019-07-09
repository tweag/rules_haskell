#!/bin/bash
bazel clean && bazel build --incompatible_depset_union=false --strategy=HaskellLinkDynamicLibrary=worker --strategy=HaskellBuildLibrary=worker --worker_sandboxing @io_tweag_rules_haskell_tutorial//lib:booleans
