"""Workspace rules (tools/repositories)"""

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_file")

def rules_haskell_worker_dependencies(**stack_kwargs):
    """
    Provide all repositories that are necessary for `rules_haskell`'s tools to
    function.
    """
    excludes = native.existing_rules().keys()

    if "rules_haskell_worker_dependencies" not in excludes:
        stack_snapshot(
            name = "rules_haskell_worker_dependencies",
            packages = [
                "base",
                "bytestring",
                "filepath",
                "ghc",
                "ghc-paths",
                "microlens",
                "process",
                "profunctors-5.5.2",
                "proto-lens-0.7.0.0",
                "proto-lens-runtime-0.7.0.0",
                "text",
                "vector",
            ],
            snapshot = "lts-20.3",
            **stack_kwargs
        )
