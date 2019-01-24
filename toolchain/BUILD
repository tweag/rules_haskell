package(default_visibility = ["//visibility:public"])

cc_library(
    name = "malloc",
)

cc_library(
    name = "stl",
)

filegroup(
    name = "empty",
    srcs = [],
)

cc_toolchain_suite(
    name = "toolchain",
    toolchains = {
        "x64_windows": ":cc-compiler-mingw64",
    },
)

cc_toolchain(
    name = "cc-compiler-mingw64",
    all_files = ":empty",
    compiler_files = ":empty",
    cpu = "x64_windows",
    dwp_files = ":empty",
    dynamic_runtime_libs = [":empty"],
    linker_files = ":empty",
    objcopy_files = ":empty",
    static_runtime_libs = [":empty"],
    strip_files = ":empty",
    supports_param_files = 0,
    toolchain_identifier = "azure-mingw64",
)

toolchain(
    name = "cc-toolchain-mingw64",
    exec_compatible_with = [
        "@bazel_tools//platforms:x86_64",
        "@bazel_tools//platforms:windows",
        "@bazel_tools//tools/cpp:mingw",
    ],
    target_compatible_with = [
        "@bazel_tools//platforms:x86_64",
        "@bazel_tools//platforms:windows",
    ],
    toolchain = ":mingw64",
    toolchain_type = "@bazel_tools//tools/cpp:toolchain_type",
)
