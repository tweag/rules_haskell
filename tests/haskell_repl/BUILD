package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
     "haskell_library",
     "haskell_repl",
)

cc_library(
  name = "cbits",
  srcs = ["cbits/Lib.c"],
)

haskell_library(
  name = "hs-lib",
  srcs = ["Foo.hs"],
  prebuilt_dependencies = ["base", "array"],
  deps = [":cbits"],
  visibility = ["//visibility:public"],
)

haskell_repl(
  name = "haskell_repl",
  target = ":hs-lib",
  # interpreted = False,
  visibility = ["//visibility:public"],
)
