load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_import",
)

haskell_import(name = "base")

haskell_library(
    name = "indef1",
    srcs = ["Data/Map.hsig", "Provide.hs"],
    deps = [":base"],
)
