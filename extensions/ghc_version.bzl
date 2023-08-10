""" Module extension which configures the GHC version"""

load(
    "@rules_haskell//haskell:private/ghc_ci.bzl",
    _ghc_version = "ghc_version",
    _ghc_default_version = "ghc_default_version",
)

def _ghc_default_version_impl(mctx):
    root_module = [ module.name for module in mctx.modules if module.is_root ]

    if root_module and root_module[0] in ['rules_haskell', 'rules_haskell_tests']:
        _ghc_version(name = "rules_haskell_ghc_version")
    else:
        _ghc_default_version(name = "rules_haskell_ghc_version")

ghc_default_version = module_extension(
    implementation = _ghc_default_version_impl,
    #environ = ["GHC_VERSION"], # Bazel >= 6.3.2
)
