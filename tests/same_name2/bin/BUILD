load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_binary", "haskell_import")

haskell_import(name = "base")

haskell_binary(
    name = "foo",
    srcs = ["Foo.hs"],
    deps = [

    # this test ensure that these two deps, with similar package:name label
    # are correctly linked as two different packages
    "//tests/same_name2/foo/b:type",
    "//tests/same_name2/foo:b-type",
    ":base",
    ],
)
