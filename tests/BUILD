package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)

haskell_binary(
    name = "hello",
    srcs = ["Main.hs"],
    deps = ["//tests/test-lib"]
)

haskell_binary(
  name = "two-libs-one-BUILD",
  main = "TogetherTest.main",
  srcs = ["TogetherTest.hs"],
  deps = [
    "//tests/together-lib:one",
    "//tests/together-lib:two"
  ]
)

[sh_test(
    name = "Run" + binary,
    srcs = ["test_binary.sh"],
    args = ["$(location %s)" % binary],
    data = [":%s" % binary],
    timeout="short",
) for binary in [
    "hello",
    "two-libs-one-BUILD",
]]
