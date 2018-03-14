package(default_testonly = 1)

# XXX This package is for manual testing. I could not find a way to
# automatically test the fact that a rule must fail (must be not a very
# common use case for Bazel) and that files produced by a failing rule have
# certain contents.

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
     "haskell_library",
     "haskell_lint",
)

haskell_library(
  name = "lib-a",
  srcs = ["Foo.hs"],
  prebuilt_dependencies = ["base"],
  visibility = ["//visibility:public"],
)

haskell_library(
  name = "lib-b",
  srcs = ["Bar.hs"],
  prebuilt_dependencies = ["base"],
  deps = [":lib-a"],
  visibility = ["//visibility:public"],
)

haskell_lint(
  name = "haskell_lint",
  deps = [":lib-b"],
  visibility = ["//visibility:public"],
)
