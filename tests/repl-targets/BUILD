package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
     "haskell_library",
     "haskell_binary",
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

haskell_library(
    name = "QuuxLib",
    srcs = ["QuuxLib.hs"],
    prebuilt_dependencies = ["base"],
)

haskell_binary(
  name = "hs-bin",
  srcs = ["Quux.hs"],
  main_file = "Quux.hs",
  deps = [":QuuxLib"],
  prebuilt_dependencies = ["base"],
  visibility = ["//visibility:public"],
)
