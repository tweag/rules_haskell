load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_library",
    "haskell_test",
    "haskell_toolchain_library",
)
load(
    "@rules_haskell//haskell:cabal.bzl",
    "haskell_cabal_binary",
    "haskell_cabal_library",
)

package(default_testonly = 1)

haskell_toolchain_library(name = "base")

haskell_cabal_library(
    name = "the_main_library",
    package_name = "package",
    srcs = glob([
        "package.cabal",
        "app/**",
        "lib/**",
        "sublib/**",
    ]),
    version = "0.1.0.0",
    deps = [
        ":base",
        ":the_sublibrary",
    ],
)

haskell_cabal_library(
    name = "the_sublibrary",
    package_name = "package",
    srcs = glob([
        "package.cabal",
        "app/**",
        "lib/**",
        "sublib/**",
    ]),
    sublibrary_name = "sublib",
    version = "0.1.0.0",
    deps = [
        ":base",
        "//tests/data:libourclibrary",
        "@stackage//:prettyprinter-compat-annotated-wl-pprint",
        "@zlib.hs//:zlib",
    ],
)

haskell_cabal_binary(
    name = "the_executable",
    srcs = glob([
        "package.cabal",
        "app/**",
        "lib/**",
        "sublib/**",
    ]),
    exe_name = "package",
    deps = [
        ":base",
        ":the_main_library",
    ],
)

haskell_test(
    name = "haskell_cabal_library_sublibrary_name",
    srcs = ["app/Main.hs"],
    deps = [
        ":base",
        ":the_main_library",
        ":the_sublibrary",
    ],
)

filegroup(
    name = "all_files",
    testonly = True,
    srcs = glob(["**"]),
    visibility = ["//visibility:public"],
)
