load(
    "@rules_haskell//haskell:cabal.bzl",
    "haskell_cabal_binary",
    "haskell_cabal_library",
)
load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_test",
    "haskell_toolchain_library",
)

haskell_toolchain_library(name = "base")

haskell_cabal_library(
    name = "the_main_library",
    package_name = "package1",
    srcs = glob([
        "package1.cabal",
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
    package_name = "package1",
    srcs = glob([
        "package1.cabal",
        "app/**",
        "lib/**",
        "sublib/**",
    ]),
    sublibrary_name = "sublib",
    version = "0.1.0.0",
    deps = [
        ":base",
    ],
)

haskell_cabal_binary(
    name = "the_executable",
    srcs = glob([
        "package1.cabal",
        "app/**",
        "lib/**",
        "sublib/**",
    ]),
    exe_name = "package1_executable",
    deps = [
        ":base",
        ":the_main_library",
    ],
)

haskell_test(
    name = "haskell_cabal_library_sublibrary_name",
    srcs = ["Main.hs"],
    visibility = ["//tests/asterius/haskell_cabal_library_sublibrary_name:__pkg__"],
    deps = [
        ":base",
        ":the_sublibrary",
    ],
)

filegroup(
    name = "all_files",
    testonly = True,
    srcs = glob(["**"]),
    visibility = ["//visibility:public"],
)
