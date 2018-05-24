load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_test")
load("@ai_formation_hazel//:hazel.bzl", "hazel_library")

haskell_test(
    name = "vector-test",
    srcs = ["vector-test.hs"],
    main_file = "vector-test.hs",
    deps = [hazel_library("vector")],
    prebuilt_dependencies = ["base"],
)

haskell_test(
    name = "old-time-test",
    srcs = ["old-time-test.hs"],
    main_file = "old-time-test.hs",
    deps = [
        hazel_library("old-time"),
    ],
    prebuilt_dependencies = ["base"],
)

