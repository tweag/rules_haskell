# Tests around our use of package names.
package(default_testonly = 1)

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_test",
)

haskell_library(
    # A package with the character "Z" in it shouldn't change its name
    name = "lib-aZ",
    srcs = ["Lib.hs"],
    prebuilt_dependencies = ["base"],
    version = "1.2.3.4",
)

haskell_test(
    name = "bin",
    srcs = ["Main.hs"],
    main_file = "Main.hs",
    prebuilt_dependencies = ["base"],
    deps = [":lib-aZ"],
)
