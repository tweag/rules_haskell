load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import_boot",
    "haskell_library",
    "haskell_test",
)

haskell_import_boot()

package(
    default_testonly = 1,
    default_visibility = ["//visibility:public"],
)

haskell_library(
    name = "one",
    srcs = ["One.hs"],
    deps = [
        ":base",
    ],
)

haskell_library(
    name = "two",
    srcs = ["Two.hs"],
    deps = [
        ":one",
        ":base",
    ],
)

haskell_test(
    name = "two-libs",
    srcs = ["Main.hs"],
    deps = [
        ":two",
        ":base",
    ],
)
