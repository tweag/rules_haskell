package(default_testonly = 1)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_test",
)

haskell_test(
    name = "binary-with-compiler-flags",
    srcs = ["Main.hs"],
    # A flag that requires -threaded, which we should get from the toolchain's
    # compiler_flags:
    compiler_flags = ["-with-rtsopts=-qg"],
    main_file = "Main.hs",
    prebuilt_dependencies = ["base"],
)
