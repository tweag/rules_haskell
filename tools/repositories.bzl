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
            local_snapshot = "//:stackage_snapshot.yaml",
            packages = [
                "base",
                "bytestring",
                "filepath",
                "ghc",
                "ghc-paths",
                "microlens",
                "process",
                "profunctors",
                "proto-lens",
                "proto-lens-runtime",
                "text",
                "vector",
            ],
            setup_deps = {
                "bifunctors" : ["@Cabal//:Cabal"],
                "proto-lens-runtime" : ["@Cabal//:Cabal"],
                "transformers-compat" : ["@Cabal//:Cabal"],
            },
            **stack_kwargs
        )
