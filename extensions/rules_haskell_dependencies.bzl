""" This module extension contains rules_haskell dependencies that are not available as modules """

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies_bzlmod")
# load("@rules_haskell//tools:os_info.bzl", "os_info")

def _rules_haskell_dependencies_impl(_mctx):
    rules_haskell_dependencies_bzlmod()
    # os_info(name = "os_info")

rules_haskell_dependencies = module_extension(
    implementation = _rules_haskell_dependencies_impl,
)
