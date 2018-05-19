package(default_testonly = 1, default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_doc",
  "haskell_cc_import",
)

haskell_cc_import(
  name = "zlib",
  shared_library = "@zlib//:lib"
)

haskell_library(
  name = "haddock-lib-deep",
  srcs = ["Deep.hsc"],
  prebuilt_dependencies = ["base"],
)

haskell_library(
  name = "haddock-lib-a",
  srcs = ["LibA.hs", "LibA/A.hs"],
  deps = [":haddock-lib-deep"],
  prebuilt_dependencies = ["base", "template-haskell"],
)

haskell_library(
  name = "haddock-lib-b",
  srcs = ["LibB.hs", "LibBHidden.hs"],
  hidden_haddock_modules = ["LibBHidden.hs"],
  deps = [":haddock-lib-a", ":zlib"],
  prebuilt_dependencies = ["base"],
)

haskell_doc(
  name = "haddock",
  deps = [":haddock-lib-b"],
  index_transitive_deps = False,
)

haskell_doc(
  name = "haddock-transitive",
  deps = [":haddock-lib-b"],
  index_transitive_deps = True,
)
