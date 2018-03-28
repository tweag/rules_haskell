package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
     "haskell_library",
     "haskell_binary",
     "haskell_test",
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
  name = "lint-lib-b",
  deps = [":lib-b"],
  visibility = ["//visibility:public"],
)

haskell_binary(
  name = "bin",
  srcs = ["Main.hs"],
  prebuilt_dependencies = ["base"],
  deps = [":lib-a"],
  visibility = ["//visibility:public"],
)

haskell_lint(
  name = "lint-bin",
  deps = [":bin"],
  visibility = ["//visibility:public"],
)
