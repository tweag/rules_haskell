""" Module extension which configures the GHC version"""

load(
    "@rules_haskell//haskell:private/ghc_ci.bzl",
    _ghc_version = "ghc_version",
)

def _ghc_version_impl(mctx):
    _ghc_version(name = "rules_haskell_ghc_version")


ghc_version = module_extension(
    implementation = _ghc_version_impl,
    #environ = ["GHC_VERSION"], # Bazel 7
)
