package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_binary",
  "haskell_library",
)

# Using a modified version due to
# https://github.com/bazelbuild/bazel/issues/4354.
# TODO revert back to upstream version once fixed.
# load("@bazel_tools//tools/build_rules:test_rules.bzl", "rule_test")
load("//third_party:test_rules.bzl", "rule_test")

load("//haskell:haskell.bzl", "haskell_toolchain")

haskell_toolchain(
  name = "ghc",
  version = "8.2.2",
  tools = "@ghc//:bin",
)

rule_test(
  name = "test-binary-simple",
  generates = ["binary-simple"],
  rule = "//tests/binary-simple",
  size = "small",
)

rule_test(
  name = "test-binary-custom-main",
  generates = ["binary-custom-main"],
  rule = "//tests/binary-custom-main",
  size = "small",
)

rule_test(
  name = "test-binary-with-lib",
  generates = ["binary-with-lib"],
  rule = "//tests/binary-with-lib",
  size = "small",
)

rule_test(
  name = "test-binary-with-prebuilt",
  generates = ["binary-with-prebuilt"],
  rule = "//tests/binary-with-prebuilt",
  size = "small",
)

rule_test(
  name = "test-binary-with-main",
  generates = ["binary-with-main"],
  rule = "//tests/binary-with-main",
  size = "small",
)

rule_test(
  name = "test-binary-with-sysdeps",
  generates = ["binary-with-sysdeps"],
  rule = "//tests/binary-with-sysdeps",
  size = "small",
)

rule_test(
  name = "test-library-deps",
  generates =
    ["library-deps-1.0.0/library-deps-1.0.0.conf",
     "library-deps-1.0.0/package.cache",
    ],
  rule = "//tests/library-deps",
  size = "small",
)

rule_test(
  name = "test-library-with-sysdeps",
  generates =
    ["library-with-sysdeps-1.0.0/library-with-sysdeps-1.0.0.conf",
     "library-with-sysdeps-1.0.0/package.cache",
    ],
  rule = "//tests/library-with-sysdeps",
  size = "small",
)

rule_test(
  name = "test-hsc",
  generates = ["hsc"],
  rule = "//tests/hsc",
  size = "small",
)

rule_test(
  name = "test-haddock",
  generates = [
    "doc-haddock-lib-b-1.0.0",
    "doc-haddock-lib-b-1.0.0/LibB.html",
    "doc-haddock-lib-b-1.0.0/doc-index.html",
    "doc-haddock-lib-b-1.0.0/haddock-lib-b-1.0.0.haddock",
    "doc-haddock-lib-b-1.0.0/haddock-lib-b.txt",
    "doc-haddock-lib-b-1.0.0/haddock-util.js",
    "doc-haddock-lib-b-1.0.0/hslogo-16.png",
    "doc-haddock-lib-b-1.0.0/index.html",
    "doc-haddock-lib-b-1.0.0/minus.gif",
    "doc-haddock-lib-b-1.0.0/ocean.css",
    "doc-haddock-lib-b-1.0.0/plus.gif",
    "doc-haddock-lib-b-1.0.0/synopsis.png",
    ],
  rule = "//tests/haddock",
  size = "small",
)

rule_test(
  name = "test-haskell_test",
  generates = ["haskell_test"],
  rule = "//tests/haskell_test",
  size = "small",
)

rule_test(
  name = "test-library-with-sysincludes",
  generates =
    ["library-with-sysincludes-1.0.0/library-with-sysincludes-1.0.0.conf",
     "library-with-sysincludes-1.0.0/package.cache",
    ],
  rule = "//tests/library-with-sysincludes",
  size = "small",
)

rule_test(
  name = "test-java_classpath",
  generates = ["java_classpath"],
  rule = "//tests/java_classpath",
  size = "small",
)

rule_test(
  name = "test-cc_haskell_import-output",
  generates = [
    "libHShs-lib-a-1.0.0-ghc8.2.2.so",
    "libHShs-lib-b-1.0.0-ghc8.2.2.so",
  ],
  rule = "//tests/cc_haskell_import:hs-lib-b-so",
  size = "small",
)

rule_test(
  name = "test-cc_haskell_import-cc-link",
  generates = ["cc-bin"],
  rule = "//tests/cc_haskell_import:cc-bin",
  size = "small",
)

sh_test(
  name = "test-haskell_binary-with-link-flags",
  size = "small",
  srcs = ["scripts/test-threaded.sh"],
  data = ["//tests/binary-with-link-flags:binary-with-link-flags"],
  args = ["$(location //tests/binary-with-link-flags:binary-with-link-flags)"]
)

rule_test(
  name = "test-lhs",
  generates = ["lhs-bin"],
  rule = "//tests/lhs:lhs-bin",
  size = "small",
)

rule_test(
  name = "test-hs-boot",
  generates = ["hs-boot"],
  rule = "//tests/hs-boot",
  size = "small",
)

rule_test(
  name = "test-textual-hdrs",
  generates = ["textual-hdrs"],
  rule = "//tests/textual-hdrs",
  size = "small",
)
