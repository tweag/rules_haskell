package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_test",
)

haskell_test(
  name = "haskell_test",
  srcs = ["Test.hs"],
  main_function = "Test.test",
  prebuilt_dependencies = ["base"],
  visibility = ["//visibility:public"],
  # Use some parameters that only test rules have.
  size = "small",
  timeout = "short",
  flaky = False
)
