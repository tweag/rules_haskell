load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

# Tests around our use of package names.
package(default_testonly = 1)

haskell_library(
    # The character "Z" should be untouched in the GHC package name.
    # However, underscores (which are not legal) should be turned into dashes.
    name = "lib-a_Z",
    srcs = ["Lib.hs"],
    version = "1.2.3.4",
    deps = ["//tests/hackage:base"],
)

haskell_test(
    name = "bin",
    size = "small",
    srcs = ["Main.hs"],
    version = "0.0.0",  # This flags triggers the `MIN_VERSION` macro generation
    deps = [
        ":lib-a_Z",
        "//tests/hackage:base",
    ],
)
