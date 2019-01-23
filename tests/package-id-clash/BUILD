load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_import_boot",
)

package(default_testonly = 1)

haskell_import_boot()

haskell_library(
    name = "sublib",
    srcs = ["Foo.hs"],
    visibility = ["//visibility:public"],
    deps = [":base"],
)

haskell_library(
    name = "lib",
    srcs = ["Baz.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":sublib",
        "//tests/package-id-clash/sublib",
        ":base",
    ],
)
