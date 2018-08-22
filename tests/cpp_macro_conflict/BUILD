package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_binary",
)

# empty library with package name "bytestring"
haskell_library(
  name = "bytestring",
  srcs = ["BS.hs"],
  deps = ["//tests:base"],
)

# This depends on two packages "bytestring"
# There should be no CPP macro conflict
haskell_binary(
  name = "macro_conflict",
  srcs = ["Main.hs"],
  deps = ["//tests:base", "//tests:bytestring", ":bytestring"],
  compiler_flags = ["-XCPP", "-Werror"],
)
