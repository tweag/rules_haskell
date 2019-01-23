load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_test",
)

package(default_testonly = 1)

haskell_test(
    name = "encoding",
    srcs = [
        "Main.hs",
        "TH.hs",
    ],
    extra_srcs = [
        "unicode.txt",
    ],
    deps = [
        "//tests/hackage:base",
        "//tests/hackage:template-haskell",
    ],
)
