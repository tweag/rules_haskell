package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
     "haskell_binary",
     "haskell_library",
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

haskell_binary(
  name = "non-top-srcs",
  sourceDir = "non-top-srcs",
  srcs = [
    "non-top-srcs/Test.hs",
    "non-top-srcs/Main.hs"
  ],
  prebuiltDeps = ["base"]
)

haskell_binary(
  name = "main-is",
  main = "MainIs.mainIs",
  srcs = [ "MainIs.hs" ],
  prebuiltDeps = ["base"],
)


haskell_library(
  name = "hsc-lib",
  sourceDir = "hsc",
  srcs = [],
  hscs = ["hsc/Test.hsc"],
  prebuiltDeps = ["base"]
)

# https://github.com/tweag/rules_haskell/issues/13
haskell_binary(
  name = "hsc",
  sourceDir = "hsc",
  srcs = ["hsc/Main.hs"],
  prebuiltDeps = ["base"],
  deps = [":hsc-lib"],
)

haskell_library(
  name = "cpphs-lib",
  sourceDir = "cpphs",
  srcs = [],
  cpphs = ["cpphs/Test.cpphs"],
  prebuiltDeps = ["base"]
)

# https://github.com/tweag/rules_haskell/issues/13
haskell_binary(
  name = "cpphs",
  sourceDir = "cpphs",
  srcs = ["cpphs/Main.hs"],
  prebuiltDeps = ["base"],
  deps = [":cpphs-lib"],
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
    "non-top-srcs",
    "main-is",
    "hsc",
    "cpphs",
]]
