exports_files(
    glob(["*.bzl"]) + [
        "assets/ghci_script",
        "private/c2hs_wrapper.sh",
        "private/ghci_repl_wrapper.sh",
        "private/haddock_wrapper.sh",
    ],
)

# to make functions visible to unit tests
exports_files(
    ["private/actions/link.bzl"],
    visibility = ["//tests/unit-tests:__pkg__"],
)

py_binary(
    name = "ls_modules",
    srcs = ["private/ls_modules.py"],
    visibility = ["//visibility:public"],
)

# toolchains must have a valid toolchain_type from bazel 0.21
toolchain_type(
    name = "toolchain",
    visibility = ["//visibility:public"],
)
