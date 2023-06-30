load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

def repositories(*, bzlmod):
    # Needed for //tests:protobuf-toolchain which is required by //rule_info:rule_info_haskell_proto.
    # rules_info_haskell_proto was not moved to rules_haskell_tests because it is in particular used by https://github.com/google/hrepl.
    stack_snapshot(
        name = "stackage",
        components = {
            "proto-lens-protoc": [
                "lib",
                "exe",
            ],
        },
        local_snapshot = "//:stackage_snapshot.yaml",
        packages = [
            # Core libraries
            "base",
            "bytestring",
            "containers",
            "deepseq",
            "mtl",
            "text",
            "vector",
            # For tests
            "lens-family-core",
            "data-default-class",
            "proto-lens",
            "proto-lens-protoc",
            "proto-lens-runtime",
            "lens-family",
        ],
        vendored_packages = {
            "ghc-paths": "@rules_haskell//tools/ghc-paths",
        },
        setup_deps = {
            # See https://github.com/tweag/rules_haskell/issues/1871
            "HUnit": ["@Cabal//:Cabal"],
            "bifunctors": ["@Cabal//:Cabal"],
            "call-stack": ["@Cabal//:Cabal"],
            "generic-deriving": ["@Cabal//:Cabal"],
            "mono-traversable": ["@Cabal//:Cabal"],
            "proto-lens-protoc": ["@Cabal//:Cabal"],
            "proto-lens-runtime": ["@Cabal//:Cabal"],
            "quickcheck-io": ["@Cabal//:Cabal"],
            "transformers-compat": ["@Cabal//:Cabal"],
            "type-errors": ["@Cabal//:Cabal"],
            "typed-process": ["@Cabal//:Cabal"],
            "unliftio-core": ["@Cabal//:Cabal"],
        },
        stack_snapshot_json = "//:stackage_snapshot.json" if not is_windows else None,
    )

def _non_module_dev_deps_2_impl(_ctx):
    repositories(bzlmod = True)

non_module_dev_deps_2 = module_extension(
    implementation = _non_module_dev_deps_2_impl,
)
