load("//skylark:lint.bzl", "skylark_lint")
load(":private/create_dummy_archive.bzl", "create_dummy_archive")

create_dummy_archive(
  name = "dummy_static_lib",
  visibility = ["//visibility:public"]
)

exports_files(
  glob(["*.bzl"]) + [
  "assets/ghci_script",

  "private/c2hs_wrapper.sh",
  "private/ghci_repl_wrapper.sh",
  "private/haddock_wrapper.sh",
])

skylark_lint()
