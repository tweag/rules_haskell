load(
    "@io_bazel_skydoc//skylark:skylark.bzl",
    "skylark_doc",
)

skylark_doc(
  name = "docs",
  srcs = [

    # The order of these files defines the order in which the corresponding
    # sections are presented in the docs.

    "//haskell:haskell.bzl",
    "//haskell:haddock.bzl",
    "//haskell:toolchain.bzl",
    "//haskell:cc.bzl",
    "//haskell:repositories.bzl",
    "//haskell:ghc_bindist.bzl",

  ],
  format = "html",
)
