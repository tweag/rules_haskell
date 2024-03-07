"""Workspace rules (tools/repositories)"""

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@rules_haskell_ghc_version//:ghc_version.bzl", "GHC_VERSION")
load("@rules_haskell//haskell:private/versions.bzl", "is_at_least")

def rules_haskell_worker_dependencies(**stack_kwargs):
    """
    Provide all repositories that are necessary for `rules_haskell`'s tools to
    function.
    """
    excludes = native.existing_rules().keys()

    if "rules_haskell_worker_dependencies" not in excludes:
        snapshot_suffix = "_{}".format(GHC_VERSION) if GHC_VERSION else ""
        stack_snapshot(
            name = "rules_haskell_worker_dependencies",
            local_snapshot = "//:stackage_snapshot{}.yaml".format(snapshot_suffix),
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
            setup_deps = {} if GHC_VERSION and is_at_least("9.6", GHC_VERSION) else {
                "bifunctors": ["@Cabal//:Cabal"],
                "proto-lens-runtime": ["@Cabal//:Cabal"],
                "transformers-compat": ["@Cabal//:Cabal"],
            },
            **stack_kwargs
        )
