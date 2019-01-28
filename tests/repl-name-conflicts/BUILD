load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

package(default_testonly = 1)

# The module in this library hides a lot of identifiers from Prelude and other
# modules used in the repl init script.
# This shouldn't break the @repl target
haskell_library(
    name = "lib",
    srcs = ["PreludeShadowing.hs"],
    deps = [
        "//tests/hackage:base",
        "//tests/hackage:bytestring",
    ],
)
