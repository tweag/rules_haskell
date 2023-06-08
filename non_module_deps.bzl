load(
    "@rules_haskell//haskell:ghc_bindist.bzl",
    "haskell_register_ghc_bindists",
)
load(
    "@rules_haskell//:constants.bzl",
    "test_ghc_version",
)

def repositories(*, bzlmod):
    # mostly for "rules_haskell_python_local"
    # Maybe registering all the toolchains is not needed here
    haskell_register_ghc_bindists(
        version = test_ghc_version,
        register = not bzlmod,
    )

def _non_module_deps_impl(_ctx):
    repositories(bzlmod = True)

non_module_deps = module_extension(
    implementation = _non_module_deps_impl,
)
