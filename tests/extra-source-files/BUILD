package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)

haskell_library(
    name = "extra-source-files",
    srcs = [
        "Foo.hs",
        "FooTH.hs",
    ],
    # Test that the linker can also see the extra_srcs.
    compiler_flags = ["-optl-Wl,@tests/extra-source-files/ld-options.txt"],
    extra_srcs = [
        "file.txt",
        "ld-options.txt",
    ],
    prebuilt_dependencies = [
        "base",
        "template-haskell",
    ],
)

haskell_binary(
    name = "extra-source-files-bin",
    srcs = [
        "FooTH.hs",
        "Foo.hs",
        "Main.hs",
    ],
    # Test that the linker can also see the extra_srcs.
    compiler_flags = ["-optl-Wl,@tests/extra-source-files/ld-options.txt"],
    extra_srcs = [
        "file.txt",
        "ld-options.txt",
    ],
    prebuilt_dependencies = [
        "base",
        "template-haskell",
    ],
)
