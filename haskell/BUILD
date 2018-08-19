load("//skylark:lint.bzl", "skylark_lint")
load(":private/dummy_linker_archive.bzl", "dummy_linker_archive")
load("@io_bazel_skydoc//skylark:skylark.bzl", "skylark_doc")

dummy_linker_archive(
    name = "dummy_static_lib",
    visibility = ["//visibility:public"],
)

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

skylark_doc(
    name = "doc_html",
    srcs = [

        # The order of these files defines the order in which the corresponding
        # sections are presented in the docs.
        "//haskell:haskell.bzl",
        "//haskell:haddock.bzl",
        "//haskell:lint.bzl",
        "//haskell:toolchain.bzl",
        "//haskell:protobuf.bzl",
        "//haskell:cc.bzl",
        "//haskell:repositories.bzl",
        "//haskell:ghc_bindist.bzl",
    ],
    format = "html",
)

skylark_lint()
