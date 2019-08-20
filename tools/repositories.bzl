"""Workspace rules (tools/repositories)"""

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

def rules_haskell_tools_dependencies():
    """Provide all repositories that are necessary for `rules_haskell`'s tools to
    function.
    """
    excludes = native.existing_rules().keys()

    if "rules_haskell_worker_dependencies" not in excludes:
        stack_snapshot(
            name = "rules_haskell_worker_dependencies",
            packages = [
                "base",
                "bytestring",
                "containers",
                "data-default-class",
                "deepseq",
                "filepath",
                "ghc",
                "ghc-paths",
                "haskell-src-exts",
                "lens-family",
                "lens-family-core",
                "microlens",
                "mtl",
                "process",
                "proto-lens",
                "text",
            ],
            snapshot = "lts-14.1",
            tools = ["@happy"],
        )

