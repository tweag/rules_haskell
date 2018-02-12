load(
    "@io_bazel_skydoc//skylark:skylark.bzl",
    "skylark_doc",
)

skylark_doc(
  name = "docs",
  srcs = [
    "//haskell:haskell.bzl",
    "//haskell:cc.bzl",
    "//haskell:haddock.bzl",
    "//haskell:toolchain.bzl"
  ],
  format = "html",
)
