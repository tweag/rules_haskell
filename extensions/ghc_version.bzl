"""Module extension which configures the default GHC version for rules_haskell and rules_haskell_tests

If used from rules_haskell or rules_haskell_tests, this will change the default
GHC version to the value of the `GHC_VERSION` environment variable (if set).

When used from other modules than rules_haskell and rules_haskell_tests, the
default GHC version is given by the `DEFAULT_GHC_VERSION` constant (see [`//haskell:ghc.bzl`](haskell/ghc.bzl).
"""

load(
    "@rules_haskell//haskell:private/ghc_ci.bzl",
    _ghc_default_version = "ghc_default_version",
    _ghc_version = "ghc_version",
)

def _ghc_default_version_impl(mctx):
    root_module = mctx.modules[0]

    if root_module.is_root and root_module.name in ["rules_haskell", "rules_haskell_tests"]:
        _ghc_version(name = "rules_haskell_ghc_version")
    else:
        _ghc_default_version(name = "rules_haskell_ghc_version")

ghc_default_version = module_extension(
    implementation = _ghc_default_version_impl,
    #environ = ["GHC_VERSION"], # Bazel >= 6.3.2
)
