package(default_visibility = ["//visibility:public"])

exports_files([
  "haskell.bzl",
  "ghc-version-check.sh",
])

load(
    "@io_bazel_skydoc//skylark:skylark.bzl",
    "skylark_doc",
)

skylark_doc(
  name = "docs",
  srcs = [
    "haskell.bzl",
    "cc.bzl",
    "haddock.bzl",
    "toolchain.bzl",
    "ghc_bindist.bzl",
  ],
  format = "html",
)
