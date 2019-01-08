load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_test",
)

haskell_import(name = "base")

haskell_test(
    name = "bin",
    srcs = ["Main.hs"],
    deps = [
        ":base",
        "//tests/package-id-clash-binary/a:foo",
        "//tests/package-id-clash-binary/b:foo",
    ],
)
