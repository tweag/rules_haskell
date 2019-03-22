load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

package(default_testonly = 1)

haskell_library(
    name = "sublib",
    srcs = ["Foo.hs"],
    visibility = ["//visibility:public"],
    deps = ["//tests/hackage:base"],
)

haskell_library(
    name = "lib",
    srcs = ["Baz.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":sublib",
        "//tests/hackage:base",
        "//tests/package-id-clash/sublib",
    ],
)
