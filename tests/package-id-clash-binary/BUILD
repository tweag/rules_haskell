load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_binary",
)

haskell_import(name = "base")

haskell_binary(
    name = "bin",
    deps = [
        ":base",
        "//tests/package-id-clash-binary/a:foo",
        "//tests/package-id-clash-binary/b:foo",
    ],
    srcs = ["Main.hs"],
)
