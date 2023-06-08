""" External repositories for the CI that need to be shared between WORKSPACE and MODULE.bazel files """

load(
    "@rules_nixpkgs_core//:nixpkgs.bzl",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)

# load("@rules_nixpkgs_python//:python.bzl", "nixpkgs_python_configure")
# load("@rules_nixpkgs_go//:go.bzl", "nixpkgs_go_configure")
# load("@rules_nixpkgs_cc//:cc.bzl", "nixpkgs_cc_configure")
load(
    "@rules_haskell_nix//:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)
load(
    "@rules_haskell//haskell:ghc_bindist.bzl",
    "haskell_register_ghc_bindists",
)

# load("//tests/integration_testing:dependencies.bzl", "integration_testing_bazel_binaries")
load(
    "@rules_haskell//:constants.bzl",
    "test_ghc_version",
)
# load("@rules_nixpkgs_nodejs//:nodejs.bzl", "nixpkgs_nodejs_configure_platforms")

# test_ghcopts = [
#     "-XStandaloneDeriving",  # Flag used at compile time
#     "-threaded",  # Flag used at link time
#     # Used by `tests/repl-flags`
#     "-DTESTS_TOOLCHAIN_COMPILER_FLAGS",
#     # this is the default, so it does not harm other tests
#     "-XNoOverloadedStrings",
# ]

# test_haddock_flags = ["-U"]

# test_repl_ghci_args = [
#     # The repl test will need this flag, but set by the local
#     # `repl_ghci_args`.
#     "-UTESTS_TOOLCHAIN_REPL_FLAGS",
#     # The repl test will need OverloadedString
#     "-XOverloadedStrings",
# ]

# test_cabalopts = [
#     # Used by `tests/cabal-toolchain-flags`
#     "--ghc-option=-DTESTS_TOOLCHAIN_CABALOPTS",
#     "--haddock-option=--optghc=-DTESTS_TOOLCHAIN_CABALOPTS",
# ] + ([
#     # To avoid ghcide linking errors with heapsize on Windows of the form
#     #
#     #   unknown symbol `heap_view_closurePtrs'
#     #
#     # See https://github.com/haskell/ghcide/pull/954
#     "--disable-library-for-ghci",
# ] if is_windows else [])

def repositories(*, bzlmod):
    nixpkgs_local_repository(
        name = "nixpkgs_default",
        nix_file = "//nixpkgs:default.nix",
    )
    nixpkgs_package(
        name = "glibc_locales",
        attribute_path = "glibcLocales",
        build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
    name = "locale-archive",
    srcs = ["lib/locale/locale-archive"],
)
""",
        repository = "@nixpkgs_default",
    )

    haskell_register_ghc_nixpkgs(
        attribute_path = "",
        #cabalopts = test_cabalopts,
        #ghcopts = test_ghcopts,
        # haddock_flags = test_haddock_flags,
        locale_archive = "@glibc_locales//:locale-archive",
        nix_file_content = """with import <nixpkgs> {}; haskell.packages.ghc925.ghc""",
        #repl_ghci_args = test_repl_ghci_args,
        repository = "@nixpkgs_default",
        version = test_ghc_version,
        register = not bzlmod,
    )

    haskell_register_ghc_bindists(
        # cabalopts = test_cabalopts,
        #ghcopts = test_ghcopts,
        version = test_ghc_version,
        register = not bzlmod,
    )

    # nixpkgs_python_configure(
    #     repository = "@nixpkgs_default",
    #     register = not bzlmod,
    # )

    #    nixpkgs_go_configure(
    #        sdk_name = "nixpkgs_go_sdk",
    #        repository = "@nixpkgs_default",
    #        register = not bzlmod,
    #        rules_go_repo_name = "io_bazel_rules_go",
    #    )

    # nixpkgs_cc_configure(
    #     # Don't override the default cc toolchain needed for bindist mode.
    #     name = "nixpkgs_config_cc",
    #     repository = "@nixpkgs_default",
    #     register = not bzlmod,
    # )

    # asterius_dependencies_nix(
    #     nix_repository = "@nixpkgs_default",
    #     nixpkgs_package_rule = nixpkgs_package,
    #     register = not bzlmod,
    # ) if is_nix_shell else asterius_dependencies_bindist(register = not bzlmod)

    # rules_haskell_asterius_toolchains(
    #     cabalopts = test_cabalopts,
    #     ghcopts = test_ghcopts,
    #     repl_ghci_args = test_repl_ghci_args,
    #     version = test_asterius_version,
    #     register = not bzlmod,
    # )

    # nixpkgs_package(
    #     name = "nixpkgs_zlib",
    #     attribute_path = "zlib",
    #     repository = "@nixpkgs_default",
    # )

#     nixpkgs_package(
#         name = "zlib.dev",
#         build_file_content = """
# load("@rules_cc//cc:defs.bzl", "cc_library")

# filegroup(
#     name = "include",
#     srcs = glob(["include/*.h"]),
#     visibility = ["//visibility:public"],
# )

# cc_library(
#     name = "zlib",
#     srcs = ["@nixpkgs_zlib//:lib"],
#     hdrs = [":include"],
#     strip_include_prefix = "include",
#     visibility = ["//visibility:public"],
#     # This rule only bundles headers and a library and doesn't compile or link by itself.
#     # We set linkstatic = 1 to quiet to quiet the following warning:
#     #
#     #   in linkstatic attribute of cc_library rule @zlib.dev//:zlib:
#     #   setting 'linkstatic=1' is recommended if there are no object files.
#     #
#     linkstatic = 1,
# )
# """,
#         repository = "@nixpkgs_default",
#     )

#     nixpkgs_package(
#         name = "nixpkgs_lz4",
#         attribute_path = "lz4.out",
#         build_file_content = """
# package(default_visibility = ["//visibility:public"])
# load("@rules_cc//cc:defs.bzl", "cc_library")

# cc_library(
# name = "nixpkgs_lz4",
# srcs = glob(["lib/liblz4.dylib", "lib/liblz4.so*"], allow_empty = True),
# includes = ["include"],
# )
#     """,
#         repository = "@nixpkgs_default",
#     )

# nixpkgs_package(
#     name = "zip",
#     attribute_path = "zip",
#     repository = "@nixpkgs_default",
# )

# nixpkgs_package(
#     name = "graphviz",
#     attribute_path = "graphviz",
#     repository = "@nixpkgs_default",
# )

# nixpkgs_package(
#     name = "sphinx",
#     attribute_path = "python39Packages.sphinx",
#     repository = "@nixpkgs_default",
# )

# nixpkgs_package(
#     name = "python3",
#     repository = "@nixpkgs_default",
# )

# nixpkgs_pandoc_configure(repository = "@nixpkgs_default")

# import_pandoc_bindists()

# integration_testing_bazel_binaries()

# For testing non standard toolchains
# The toolchain_libraries rule provide a default value for the toolchain_libraries
# variable, so we can load it even if we are not on linux.
# toolchain_libraries(
#     name = "toolchains_libraries",
#     repository = "linux_amd64_asterius" if is_linux else "",
# )

# nixpkgs_nodejs_configure_platforms(
#     name = "nixpkgs_nodejs",
#     repository = "@nixpkgs_default",
#     register = not bzlmod,
# )

def _non_module_deps_1_impl(_ctx):
    repositories(bzlmod = True)

non_module_deps_1 = module_extension(
    implementation = _non_module_deps_1_impl,
)
