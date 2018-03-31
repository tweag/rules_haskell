package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
     "haskell_library",
     "haskell_doctest",
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

haskell_doctest(
  name = "haskell_doctest",
  deps = [":lib-b"],
  visibility = ["//visibility:public"],
)
