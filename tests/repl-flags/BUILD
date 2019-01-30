load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_test")

package(default_testonly = 1)

# This test the `compiler` flags in many ways:
# - the test needs to be built with `-DTESTS_TOOLCHAIN_COMPILER_FLAGS`, provided by the toolchain attribute `compiler_flags`
# - the test needs `-XOverloadedStrings`, provided by this rule `compiler_flags`
# - toolchain and rule `compiler_flags` are additive, else one of the previous test will fail
# - the ordering is as such as rule flags are more important that toolchain flags

# This rule must build correctly (using `bazel build`), but also as a repl (using `bazel run //tests/repl-flags:compiler_flags@repl`)
haskell_test(
    name = "compiler_flags",
    srcs = ["CompilerFlags.hs"],

    # This also ensure that local `compiler_flags` does not override the `global ones`
    compiler_flags = ["-XOverloadedStrings"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/hackage:base",
        "//tests/hackage:bytestring",
    ],
)

# This test the `repl_ghci_args` flags in a similar way
# - the test needs to be built with `-DTESTS_TOOLCHAIN_REPL_FLAGS`, provided by the toolchain attribute `compiler_flags`
# - the test needs `-XOverloadedStrings`, provided by toolchain `repl_ghci_args`
# - toolchain and rule `compiler_flags` are additive, else one of the previous test will fail
# - the ordering is as such as rule flags are more important that
#    toolchain flags and that repl flags are more important that
#    copmiler flags

# This rule must build correctly (using `bazel build`), but also as a repl (using `bazel run //tests/repl-flags:compiler_flags@repl`). The final result between the repl and the binary must be different
haskell_test(
    name = "repl_flags",
    srcs = ["ReplFlags.hs"],

    # This also ensure that local `repl_flags` does not override the `global ones`
    repl_ghci_args = ["-DTESTS_TOOLCHAIN_REPL_FLAGS"],
    visibility = ["//visibility:public"],
    deps = [
        "//tests/hackage:base",
        "//tests/hackage:bytestring",
    ],
)
