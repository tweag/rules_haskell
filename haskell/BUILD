exports_files(
    glob(["*.bzl"]) + [
        "assets/ghci_script",
        "private/c2hs_wrapper.sh",
        "private/ghci_repl_wrapper.sh",
        "private/haddock_wrapper.sh",
    ],
)

py_binary(
    name = "ls_modules",
    srcs = ["private/ls_modules.py"],
    visibility = ["//visibility:public"],
)
