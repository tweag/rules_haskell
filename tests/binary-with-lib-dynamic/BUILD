load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

haskell_library(
    name = "lib",
    srcs = glob(["src/*.hs"]),
    linkstatic = False,
    src_strip_prefix = "src",
)

haskell_test(
    name = "binary-with-lib-dynamic",
    srcs = ["Main.hs"],
    linkstatic = False,
    visibility = ["//visibility:public"],
    deps = [
        ":lib",
        "//tests/hackage:base",
    ],
)
