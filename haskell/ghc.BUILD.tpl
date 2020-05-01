load(
    "@rules_cc//cc:defs.bzl",
    "cc_toolchain",
    "cc_toolchain_suite",
)
load(
    "@rules_haskell//haskell:cc_toolchain_config.bzl",
    "cc_toolchain_config",
)
load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_import",
    "haskell_toolchain",
)

package(default_visibility = ["//visibility:public"])

%{toolchain}

filegroup(
    name = "bin",
    srcs = glob(["bin/*"]),
)

# Expose embedded MinGW toolchain when on Windows.

filegroup(
    name = "mingw",
    srcs = glob(["mingw/**"], allow_empty = True),
)

cc_toolchain_suite(
    name = "cc_toolchain",
    toolchains = {
        "x64_windows": ":cc-compiler-mingw64",
        "x64_windows|ghc-mingw-gcc": ":cc-compiler-mingw64",
    },
)

# Keep in sync with @bazel_tools//cpp:cc-compiler-x64_windows definition.
cc_toolchain(
    name = "cc-compiler-mingw64",
    all_files = ":mingw",
    ar_files = ":mingw",
    as_files = ":mingw",
    compiler_files = ":mingw",
    dwp_files = ":mingw",
    linker_files = ":mingw",
    objcopy_files = ":mingw",
    strip_files = ":mingw",
    supports_param_files = 0,
    toolchain_config = ":ghc_windows_mingw64_config",
    toolchain_identifier = "ghc_windows_mingw64",
)

cc_toolchain_config(name = "ghc_windows_mingw64_config")
