load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
    "haskell_cc_import",
    "haskell_import",
)

# cc_binary(
#     name = "cc_youpi",
#     deps = ["@lz4//:lib3"],
#     srcs = ["LZ4Main.c"],
#     testonly=1,
# )

haskell_import(name = "base")

haskell_cc_import(
    name = "lz4",
    shared_library = "@lz4//:lib3",
    testonly=1,
)


haskell_binary(
    name = "youpi",
    deps = [":base", ":lz4"],
    srcs = ["LZ4Main.hs"],
    testonly=1,
)
