load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import_boot",
    "haskell_library",
)

package(default_testonly = 1)

haskell_import_boot()

# The module in this library hides a lot of identifiers from Prelude and other
# modules used in the repl init script.
# This shouldn't break the @repl target
haskell_library(
    name = "lib",
    srcs = ["PreludeShadowing.hs"],
    deps = [
        ":base",
        "@hackage//:bytestring",
    ],
)
