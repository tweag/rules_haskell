exports_files(
    glob(["*.bzl"]) + [
        "assets/ghci_script",
        "private/ghci_repl_wrapper.sh",
        "private/runghc_wrapper.sh",
        "private/haddock_wrapper.sh.tpl",
    ],
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

toolchain_type(
    name = "doctest-toolchain",
    visibility = ["//visibility:public"],
)
