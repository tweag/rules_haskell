load("//skylark:lint.bzl", "skylark_lint")

exports_files(
  glob(["*.bzl"]) + [
  "assets/cpp_defines",
  "assets/ghci_script",

  "private/ghci_repl_wrapper.sh",
  "private/haddock_wrapper.sh",
])

skylark_lint()
