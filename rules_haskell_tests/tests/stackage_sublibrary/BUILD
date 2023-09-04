load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_test",
    "haskell_toolchain_library",
)

package(default_testonly = 1)

haskell_toolchain_library(name = "base")

# We directly depend on the "sublib" sublibrary of the "package1" package.
haskell_test(
    name = "stackage_sublibrary",
    srcs = ["Main.hs"],
    deps = [
        ":base",
        "@stackage-pinning-test//package1",
        "@stackage-pinning-test//package1:sublib",
    ],
)
