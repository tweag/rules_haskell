load("//skylark:lint.bzl", "skylark_lint")

exports_files(
  glob(["*.bzl"]) + [
  "assets/ghci_script",

  "private/c2hs_wrapper.sh",
  "private/ghci_repl_wrapper.sh",
  "private/haddock_wrapper.sh",
])

skylark_lint()
