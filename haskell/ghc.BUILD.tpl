load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_toolchain",
)

%{toolchain}

filegroup(
    name = "bin",
    srcs = glob(["bin/*"]),
)

# Expose embedded MinGW toolchain when on Windows.

filegroup(
    name = "empty",
    srcs = [],
)

cc_toolchain_suite(
    name = "cc_toolchain",
    toolchains = {
        "x64_windows": ":cc-compiler-mingw64",
        "x64_windows|ghc-mingw-gcc": ":cc-compiler-mingw64",
    },
    visibility = ["//visibility:public"],
)

# Keep in sync with @bazel_tools//cpp:cc-compiler-x64_windows definition.
cc_toolchain(
    name = "cc-compiler-mingw64",
    all_files = ":empty",
    ar_files = ":empty",
    as_files = ":empty",
    compiler_files = ":empty",
    cpu = "x64_windows",
    dwp_files = ":empty",
    linker_files = ":empty",
    objcopy_files = ":empty",
    strip_files = ":empty",
    supports_param_files = 0,
    toolchain_identifier = "ghc_windows_mingw64",
    visibility = ["//visibility:public"],
)
