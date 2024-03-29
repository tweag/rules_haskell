# This file constructs a dummy workspace to test
# haskell binaries that are included from outside repositories
# (because linking external repositories works differently).

# Repo-ception, in the sense that we build a WORKSPACE
# that references the workspaces already set up in the
# `rules_haskell` WORKSPACE.
def _haskell_package_repository_dummy_impl(rep_ctx):
    rep_ctx.file(
        "WORKSPACE",
        executable = False,
        content = """
repository(name={name})

register_toolchains(
  "@rules_haskell_tests//tests/:ghc"
)
""".format(name = rep_ctx.name),
    )

    # this mirrors tests/library-with-cbits

    rep_ctx.file(
        "BUILD",
        executable = False,
        content = """
load(
  "@rules_haskell//haskell:defs.bzl",
  "haskell_toolchain",
  "haskell_library",
)
load(
  "@rules_haskell//:constants.bzl",
  "test_ghc_version",
)

haskell_library(
  name = "library-with-cbits",
  srcs = ["AddOne.hs"],
  deps = [
      "@rules_haskell_tests//tests/data:ourclibrary",
      "@rules_haskell_tests//tests/hackage:base",
  ],

  linkstatic = False,
  visibility = ["//visibility:public"],
)
""",
    )

    rep_ctx.file(
        "AddOne.hs",
        executable = False,
        content = """
module AddOne where

foreign import ccall "c_add_one" addOne :: Int -> Int
""",
    )

haskell_package_repository_dummy = repository_rule(
    _haskell_package_repository_dummy_impl,
    local = True,
)
