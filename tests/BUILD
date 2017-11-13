package(default_testonly = 1)

load("//haskell:haskell.bzl",
    "haskell_library",
    "haskell_binary",
)

haskell_binary(
    name = "hello",
    srcs = ["hello.hs"],
    # main_is = "hello.hs",
)

[sh_test(
    name = "Run" + binary,
    srcs = ["test_binary.sh"],
    args = ["$(location %s)" % binary],
    data = [":%s" % binary],
) for binary in [
    "hello",
]]
