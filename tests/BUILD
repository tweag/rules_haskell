package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

haskell_binary(
    name = "hello",
    srcs = ["Main.hs"],
    deps = ["//tests/test-lib:test-lib"]
)

[sh_test(
    name = "Run" + binary,
    srcs = ["test_binary.sh"],
    args = ["$(location %s)" % binary],
    data = [":%s" % binary],
) for binary in [
    "hello",
]]
