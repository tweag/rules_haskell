package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_binary",
  "haskell_lint",
)

load(
  "@ai_formation_hazel//:hazel.bzl",
  "hazel_library",
)

haskell_binary(
  name = "cat_hs",
  src_strip_prefix = "src",
  srcs = glob(["src/**/*.hs"]),
  deps = [
    hazel_library("base"),
    "//lib/args",
    "//lib/cat",
  ],
)

haskell_lint(
  name = "lint",
  deps = [":cat_hs"],
)
