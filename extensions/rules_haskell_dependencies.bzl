""" This module extension contains rules_haskell dependencies that are not available as modules """

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies_bzlmod")
load("@rules_haskell//tools:os_info.bzl", "os_info")
load("@rules_haskell//tools:repositories.bzl", "rules_haskell_worker_dependencies")
load("@rules_haskell_ghc_version//:ghc_version.bzl", "GHC_VERSION")

def repositories(*, bzlmod):  # @unused
    rules_haskell_dependencies_bzlmod()

    # Some helpers for platform-dependent configuration
    maybe(
        os_info,
        name = "os_info",
    )

    # For persistent worker (tools/worker)
    # TODO: make this customizable via a module extension so that users
    # of persistant workers can use dependencies compatible with the
    # selected toolchain.
    rules_haskell_worker_dependencies()

    if GHC_VERSION and GHC_VERSION.startswith("9.4."):
        http_archive(
            name = "Cabal",
            build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
haskell_cabal_library(
    name = "Cabal",
    srcs = glob(["Cabal/**"]),
    verbose = False,
    version = "3.8.1.0",
    visibility = ["//visibility:public"],
)
""",
            sha256 = "b697b558558f351d2704e520e7dcb1f300cd77fea5677d4b2ee71d0b965a4fe9",
            strip_prefix = "cabal-ghc-9.4-paths-module-relocatable",
            urls = ["https://github.com/tweag/cabal/archive/refs/heads/ghc-9.4-paths-module-relocatable.zip"],
        )
    else:
        http_archive(
            name = "Cabal",
            build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
haskell_cabal_library(
    name = "Cabal",
    srcs = glob(["Cabal/**"]),
    verbose = False,
    version = "3.10.3.0",
    visibility = ["//visibility:public"],
)
""",
            sha256 = "be8460bde59089b99caa6d6f4ae3bbf6f92019c8634aad3d7edc5134a642bb24",
            strip_prefix = "cabal-Cabal-v3.10.3.0",
            urls = ["https://github.com/haskell/cabal/archive/refs/tags/Cabal-v3.10.3.0.zip"],
        )

def _rules_haskell_dependencies_impl(_mctx):
    repositories(bzlmod = True)

rules_haskell_dependencies = module_extension(
    implementation = _rules_haskell_dependencies_impl,
)
