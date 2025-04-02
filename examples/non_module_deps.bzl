""" External repositories for the CI that need to be shared between WORKSPACE and MODULE.bazel files """

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@rules_haskell//tools:os_info.bzl", "os_info")

def repositories(*, bzlmod):
    # Some helpers for platform-dependent configuration
    os_info(name = "os_info")

    # For the cat_hs example.
    http_archive(
        name = "zlib.hs",
        build_file_content = """
load("@rules_cc//cc:defs.bzl", "cc_library")
cc_library(
    name = "zlib.hs",
    # Import `:z` as `srcs` to enforce the library name `libz.so`. Otherwise,
    # Bazel would mangle the library name and e.g. Cabal wouldn't recognize it.
    srcs = [":z"],
    hdrs = glob(["*.h"]),
    includes = ["."],
    visibility = ["//visibility:public"],
)
cc_library(
    name = "z",
    srcs = glob(["*.c"]),
    hdrs = glob(["*.h"]),
    copts = select({
        "@bazel_tools//src/conditions:windows": [],
        # Needed to avoid "call to undeclared function" errors [-Wimplicit-function-declaration]
        "//conditions:default": ["-DZ_HAVE_UNISTD_H"],
    }),
)
""",
        sha256 = "9a93b2b7dfdac77ceba5a558a580e74667dd6fede4585b91eefb60f03b72df23",
        strip_prefix = "zlib-1.3.1",
        urls = ["https://github.com/madler/zlib/releases/download/v1.3.1/zlib-1.3.1.tar.gz"],
    )

    # Demonstrates a vendored Stackage package to bump a version bound.
    http_archive(
        name = "split",
        build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@stackage//:packages.bzl", "packages")
haskell_cabal_library(
    name = "split",
    version = packages["split"].version,
    srcs = glob(["**"]),
    deps = packages["split"].deps,
    visibility = ["//visibility:public"],
)
    """,
        patch_args = ["-p1"],
        patches = ["@rules_haskell_examples//:split.patch"],
        sha256 = "1dcd674f7c5f276f33300f5fd59e49d1ac6fc92ae949fd06a0f6d3e9d9ac1413",
        strip_prefix = "split-0.2.3.3",
        urls = ["http://hackage.haskell.org/package/split-0.2.3.3/split-0.2.3.3.tar.gz"],
    )

    # TODO: Remove when tests are run with a ghc version containing Cabal >= 3.10
    # See https://github.com/tweag/rules_haskell/issues/1871
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

def _non_module_deps_impl(_ctx):
    repositories(bzlmod = True)

non_module_deps = module_extension(
    implementation = _non_module_deps_impl,
)
