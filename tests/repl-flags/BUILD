package(default_visibility = ["//visibility:public"])

load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_binary", "haskell_toolchain")



# This test the `compiler` flags in many ways:
# - the test needs to be built with `-DTESTS_TOOLCHAIN_COMPILER_FLAGS`, provided by the toolchain attribute `compiler_flags`
# - the test needs `-XOverloadedStrings`, provided by this rule `compiler_flags`
# - toolchain and rule `compiler_flags` are additive, else one of the previous test will fail
# - the ordering is as such as rule flags are more important that toolchain flags

# This rule must build correctly (using `bazel build`), but also as a repl (using `bazel run //tests/repl-flags:compiler_flags-repl`)
haskell_binary(
   name = "compiler_flags",
   srcs = ["CompilerFlags.hs"],
   main_file = "CompilerFlags.hs",
   visibility = ["//visibility:public"],

   # This also ensure that local `compiler_flags` does not override the `global ones`
   compiler_flags = ["-XOverloadedStrings"],

   prebuilt_dependencies = ["base", "bytestring"],
)