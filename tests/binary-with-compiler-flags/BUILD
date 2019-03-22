load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_test",
)

package(default_testonly = 1)

haskell_test(
    name = "binary-with-compiler-flags",
    srcs = ["Main.hs"],
    # Flags that require -threaded, which we should get from the toolchain's
    # compiler_flags. Include spaces to validate proper quoting:
    compiler_flags = [
        "-with-rtsopts=-N2 -qg -I0 -n2m -A128m",
        "-XLambdaCase",
    ],
    deps = ["//tests/hackage:base"],
)
