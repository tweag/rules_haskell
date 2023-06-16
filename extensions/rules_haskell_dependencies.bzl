""" This module extension contains rules_haskell dependencies that are not available as modules """

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies_bzlmod")

def _rules_haskell_dependencies_impl(_mctx):
    rules_haskell_dependencies_bzlmod()

rules_haskell_dependencies = module_extension(
    implementation = _rules_haskell_dependencies_impl,
)
