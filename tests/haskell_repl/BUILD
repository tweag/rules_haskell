package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
     "haskell_library",
     "haskell_binary",
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
  name = "haskell_lib_repl",
  target = ":hs-lib",
  visibility = ["//visibility:public"],
)

haskell_binary(
  name = "hs-bin",
  srcs = ["Quux.hs"],
  main_file = "Quux.hs",
  prebuilt_dependencies = ["base"],
  visibility = ["//visibility:public"],
)

haskell_repl(
  name = "haskell_bin_repl",
  target = ":hs-bin",
  visibility = ["//visibility:public"],
)
