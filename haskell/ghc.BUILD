package(default_visibility = ["//visibility:public"])

filegroup(
    name = "bin",
    srcs = glob(["bin/*"]),
)

cc_library(
    name = "threaded-rts",
    srcs = glob(
        ["lib/ghc-*/rts/libHSrts_thr-ghc*." + ext for ext in [
            "so",
            "dylib",
        ]] +
        # dependency of `libHSrts_thr_ghc*`
        # globbing on the `so` version to stay working when they update
        [
            "lib/ghc-*/rts/libffi.so.*",
        ],
    ),
    hdrs = glob(["lib/ghc-*/include/**/*.h"]),
    strip_include_prefix = glob(
        ["lib/ghc-*/include"],
        exclude_directories = 0,
    )[0],
)

# TODO: detect this more automatically.
cc_library(
    name = "unix-includes",
    hdrs = glob(["lib/ghc-*/unix-*/include/*.h"]),
    includes = glob(
        ["lib/ghc-*/unix-*/include"],
        exclude_directories = 0,
    ),
)

# This is needed for Hazel targets.
cc_library(
    name = "rts-headers",
    hdrs = glob([
        "lib/ghc-*/include/**/*.h",
        "lib/include/**/*.h",
    ]),
    includes = glob(
        [
            "lib/ghc-*/include",
            "lib/include",
        ],
        exclude_directories = 0,
    ),
)

# Expose embedded MinGW toolchain when on Windows.

filegroup(
    name = "empty",
    srcs = [],
)

cc_toolchain_suite(
    name = "toolchain",
    toolchains = {
        "x64_windows": ":cc-compiler-mingw64",
        "x64_windows|ghc-mingw-gcc": ":cc-compiler-mingw64",
    },
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
)
