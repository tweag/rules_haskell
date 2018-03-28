package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_test",
  "haskell_library",
)

cc_library(
  name = "cbits",
  srcs = ["cbits.c"],
)

haskell_library(
  name = "mylib",
  srcs = ["Lib.hs"],
  prebuilt_dependencies = ["base"],
)

haskell_test(
  name = "haskell_test",
  srcs = ["Test.hs"],
  main_function = "Test.test",
  prebuilt_dependencies = ["base"],
  deps = [":cbits", ":mylib"],
  visibility = ["//visibility:public"],
  # Use some parameters that only test rules have.
  size = "small",
  timeout = "short",
  flaky = False
)
