load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_import(name = "base")

haskell_import(name = "transformers")

haskell_library(
    name = "lib",
    srcs = ["Lib.hs"],
    deps = [
        ":base",
        ":transformers",
    ],
)

haskell_test(
    name = "binary-with-import",
    srcs = ["Main.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":base",
        ":lib",
    ],
)
