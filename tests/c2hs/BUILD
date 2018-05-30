package(default_testonly = 1)

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_c2hs",
  "haskell_library",
)

haskell_c2hs(
  name = "c2hs",
  srcs = [
    "Foo.chs",
    "Bar.chs",
  ],
)

haskell_library(
  name = "c2hs-consumer",
  srcs = [":c2hs"],
  prebuilt_dependencies = [
    "base",
  ],
)
