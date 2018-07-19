# Tests around our use of package names.
package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)

haskell_library(
    # The character "Z" should be untouched in the GHC package name.
    # However, underscores (which are not legal) should be turned into dashes.
    name = "lib-a_Z",
    srcs = ["Lib.hs"],
    prebuilt_dependencies = ["base"],
    version = "1.2.3.4",
)

haskell_test(
    name = "bin",
    size = "small",
    srcs = ["Main.hs"],
    main_file = "Main.hs",
    prebuilt_dependencies = ["base"],
    deps = [":lib-a_Z"],
)
