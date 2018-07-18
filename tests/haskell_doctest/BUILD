package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
     "haskell_library",
     "haskell_binary",
     "haskell_doctest",
)

cc_library(
  name = "cbits",
  srcs = ["cbits.c"],
)

haskell_library(
  name = "lib-a",
  srcs = ["Foo.hs"],
  deps = [
    "@zlib.dev//:zlib",
    ":cbits",
  ],
  prebuilt_dependencies = ["base"],
)

haskell_library(
  name = "lib-b",
  srcs = ["Bar.hs", "Baz.hs", "Quux.hsc"],
  deps = [":lib-a"],
  prebuilt_dependencies = ["base"],
)

haskell_doctest(
  name = "doctest-lib-all-fail",
  deps = [":lib-b"],
  tags = ["manual"], # must FAIL
  visibility = ["//visibility:public"],
)

haskell_doctest(
  name = "doctest-lib-all-success",
  deps = [":lib-b"],
  doctest_flags = ["-DMAGIC_DOCTEST_THING"],
  visibility = ["//visibility:public"],
)

haskell_doctest(
  name = "doctest-lib",
  deps = [":lib-b"],
  modules = ["Bar"], # exclude Baz and succeed
  visibility = ["//visibility:public"],
)

haskell_binary(
  name = "bin",
  srcs = ["Main.hs"],
  deps = [":lib-a"],
  prebuilt_dependencies = ["base"],
)

haskell_doctest(
  name = "doctest-bin",
  deps = [":bin"],
  visibility = ["//visibility:public"],
)
