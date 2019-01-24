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
    # TODO: Broken with ghc_bindist toolchain:
    # tests/encoding/Main.hs:8:17: error:
    #   * Exception when trying to run compile-time code:
    #      tests/encoding/unicode.txt: hGetContents: invalid argument (invalid byte sequence)
    tags = ["ghcbindist_th_failure"],
    deps = [
        "//tests/hackage:base",
        "//tests/hackage:template-haskell",
    ],
)
