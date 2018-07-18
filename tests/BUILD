package(default_testonly = 1)

load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_binary",
  "haskell_library",
  "haskell_toolchain",
  "haskell_doctest_toolchain",
  "haskell_proto_toolchain",
)
load("@bazel_tools//tools/build_rules:test_rules.bzl", "rule_test")
load("//skylark:lint.bzl", "skylark_lint")

haskell_toolchain(
  name = "ghc",
  version = "8.2.2",
  tools = "@ghc//:bin",
  c2hs = "@c2hs//:bin",
  # This toolchain is morally testonly.  However, that would break
  # our tests of haskell_library_rules: aspects of non-testonly
  # proto_library rules (from com_google_protobuf) can't themselves
  # be testonly.
  testonly = 0,
  compiler_flags = [
      "-XStandaloneDeriving", # Flag used at compile time
      "-threaded",            # Flag used at link time

     # Used by `tests/repl-flags`
      "-DTESTS_TOOLCHAIN_COMPILER_FLAGS",
      "-XNoOverloadedStrings", # this is the default, so it does not harm other tests
  ],

  repl_ghci_args = [
      # Used by `tests/repl-flags`
      "-UTESTS_TOOLCHAIN_REPL_FLAGS", # The repl test will need this flag, but set by the local `repl_ghci_args`.
      "-XOverloadedStrings" # The repl test will need OverloadedString
     ],
  locale_archive = select({
    # For some reason glibcLocales is not available on Darwin.
    "@bazel_tools//src/conditions:darwin": None,
    "//conditions:default": "@glib_locales//:locale-archive",
  }),
)

haskell_doctest_toolchain(
  name = "doctest-toolchain",
  doctest = "@doctest//:bin",
)

haskell_proto_toolchain(
  name = "protobuf-toolchain",
  protoc = "@com_google_protobuf//:protoc",
  plugin = "@protoc_gen_haskell//:bin/proto-lens-protoc",
  prebuilt_deps = [
    "base",
    "bytestring",
    "containers",
    "data-default-class",
    "lens-family",
    "lens-labels",
    "proto-lens",
    "text",
  ],
  testonly = 0,
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

sh_test(
  name = "test-binary-with-data",
  size = "small",
  srcs = ["//tests/binary-with-data"],
  data = ["//tests/binary-with-data:bin1"],
  args = ["$(location //tests/binary-with-data:bin1)"],
)

rule_test(
  name = "test-library-deps",
  generates =
    ["testsZSlibrary-depsZSlibrary-deps-1.0.0/testsZSlibrary-depsZSlibrary-deps-1.0.0.conf",
     "testsZSlibrary-depsZSlibrary-deps-1.0.0/package.cache",
    ],
  rule = "//tests/library-deps",
  size = "small",
)

rule_test(
  name = "test-library-with-sysdeps",
  generates =
    ["testsZSlibrary-with-sysdepsZSlibrary-with-sysdeps-1.0.0/testsZSlibrary-with-sysdepsZSlibrary-with-sysdeps-1.0.0.conf",
     "testsZSlibrary-with-sysdepsZSlibrary-with-sysdeps-1.0.0/package.cache",
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
    "haddock/index",
    "haddock/testsZShaddockZShaddock-lib-a-1.0.0",
    "haddock/testsZShaddockZShaddock-lib-b-1.0.0",
    "haddock/testsZShaddockZShaddock-lib-deep-1.0.0"
  ],
  rule = "//tests/haddock",
  size = "small",
)

rule_test(
  name = "test-haskell_lint-library",
  generates = [
    "lint-log-lib-b",
  ],
  rule = "//tests/haskell_lint:lint-lib-b",
  size = "small",
)

rule_test(
  name = "test-haskell_lint-binary",
  generates = [
    "lint-log-bin",
  ],
  rule = "//tests/haskell_lint:lint-bin",
  size = "small",
)

rule_test(
  name = "test-haskell_proto_library",
  generates = [
    "testsZShaskellZUprotoZUlibraryZShs-lib-1.0.0/package.cache",
    "testsZShaskellZUprotoZUlibraryZShs-lib-1.0.0/testsZShaskellZUprotoZUlibraryZShs-lib-1.0.0.conf"
  ],
  rule = "//tests/haskell_proto_library:hs-lib",
  size = "small",
)

rule_test(
  name = "test-haskell_doctest",
  generates = [
    "doctest-log-doctest-lib-testsZShaskellZUdoctestZSlib-b-1.0.0",
  ],
  rule = "//tests/haskell_doctest:doctest-lib",
  size = "small",
)

rule_test(
  name = "test-haskell_test",
  generates = ["haskell_test"],
  rule = "//tests/haskell_test:haskell_test",
  size = "small",
)

rule_test(
  name = "test-hidden-modules",
  generates = [
    "testsZShidden-modulesZSlib-c-1.0.0/testsZShidden-modulesZSlib-c-1.0.0.conf",
    "testsZShidden-modulesZSlib-c-1.0.0/package.cache"
  ],
  rule = "//tests/hidden-modules:lib-c",
  size = "small",
)

rule_test(
  name = "test-library-with-sysincludes",
  generates =
    ["testsZSlibrary-with-sysincludesZSlibrary-with-sysincludes-1.0.0/testsZSlibrary-with-sysincludesZSlibrary-with-sysincludes-1.0.0.conf",
     "testsZSlibrary-with-sysincludesZSlibrary-with-sysincludes-1.0.0/package.cache",
    ],
  rule = "//tests/library-with-sysincludes",
  size = "small",
)

rule_test(
  name = "test-package-id-clash",
  generates =
    ["testsZSpackage-id-clashZSlib-1.0.0/testsZSpackage-id-clashZSlib-1.0.0.conf",
     "testsZSpackage-id-clashZSlib-1.0.0/package.cache",
    ],
  rule = "//tests/package-id-clash:lib",
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
  generates = select({
      "@bazel_tools//src/conditions:darwin":
          ["libHStestsZSccZUhaskellZUimportZShs-lib-a-1.0.0-ghc8.2.2.dylib",
           "libHStestsZSccZUhaskellZUimportZShs-lib-b-1.0.0-ghc8.2.2.dylib",
          ],
      "//conditions:default":
          ["libHStestsZSccZUhaskellZUimportZShs-lib-a-1.0.0-ghc8.2.2.so",
           "libHStestsZSccZUhaskellZUimportZShs-lib-b-1.0.0-ghc8.2.2.so",
          ]}),
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
  rule = "//tests/hs-boot:hs-boot",
  size = "small",
)

rule_test(
  name = "test-textual-hdrs",
  generates = ["textual-hdrs"],
  rule = "//tests/textual-hdrs:textual-hdrs",
  size = "small",
)

rule_test(
  name = "test-two-libs",
  generates = ["two-libs"],
  rule = "//tests/two-libs:two-libs",
  size = "small",
)

genrule(
  name = "run-bin-with-lib",
  outs = ["dummy"],
  tools = ["//tests/binary-with-lib:binary-with-lib"],
  cmd = """sh -c '
    set -e
    $(location //tests/binary-with-lib:binary-with-lib)
    touch $(location dummy)
'""",
)

rule_test(
  name = "test-run-bin-with-lib",
  generates = ["dummy"],
  rule = "//tests:run-bin-with-lib",
  size = "small",
)

genrule(
  name = "run-bin-with-c-lib",
  outs = ["c-dummy"],
  tools = ["//tests/c-compiles"],
  cmd = """sh -c '
    set -e
    $(location //tests/c-compiles)
    touch $(location c-dummy)
'"""
)

rule_test(
  name = "test-run-bin-with-c-lib",
  generates = ["c-dummy"],
  rule = "//tests:run-bin-with-c-lib",
  size = "small",
)

skylark_lint()
