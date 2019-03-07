package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_lint",
  "haskell_test",
)

load(
  "@ai_formation_hazel//:hazel.bzl",
  "hazel_library",
)

haskell_library(
  name = "args",
  src_strip_prefix = "src",
  srcs = glob(["src/**/*.hs"]),
  deps = [
    hazel_library("base"),
    hazel_library("optparse-applicative"),
  ],
)

haskell_test(
  name = "unit",
  src_strip_prefix = "src",
  srcs = glob(["test/**/*.hs"]),
  deps = [
    hazel_library("base"),
    hazel_library("hspec"),
    hazel_library("optparse-applicative"),
    ':args',
  ],
)

haskell_lint(
  name = "lint",
  deps = [":args"],
)
