workspace(name = "io_tweag_rules_haskell")

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

load(":nix-workspace.bzl", "nix_packages")
nix_packages()

http_archive(
  name = "com_google_protobuf",
  sha256 = "cef7f1b5a7c5fba672bec2a319246e8feba471f04dcebfe362d55930ee7c1c30",
  strip_prefix = "protobuf-3.5.0",
  urls = ["https://github.com/google/protobuf/archive/v3.5.0.zip"],
)

register_toolchains(
  "//tests:ghc",
  "//tests:protobuf-toolchain",
)


# zlib as a Haskell library

new_http_archive(
  name = "haskell_zlib",
  build_file = "tests/BUILD.zlib",
  strip_prefix = "zlib-0.6.2",
  urls = ["https://hackage.haskell.org/package/zlib-0.6.2/zlib-0.6.2.tar.gz"],
)

maven_jar(
  name = "org_apache_spark_spark_core_2_10",
  artifact = "org.apache.spark:spark-core_2.10:1.6.0",
)

# For Skydoc

http_archive(
    name = "io_bazel_rules_sass",
    strip_prefix = "rules_sass-0.0.3",
    urls = ["https://github.com/bazelbuild/rules_sass/archive/0.0.3.tar.gz"],
)
load("@io_bazel_rules_sass//sass:sass.bzl", "sass_repositories")
sass_repositories()

http_archive(
    name = "io_bazel_skydoc",
    strip_prefix = "skydoc-b374449408e759e32e010fa6a20585fe9fabd523",
    urls = ["https://github.com/mrkkrp/skydoc/archive/b374449408e759e32e010fa6a20585fe9fabd523.tar.gz"],
)
load("@io_bazel_skydoc//skylark:skylark.bzl", "skydoc_repositories")
skydoc_repositories()

# For buildifier

# XXX Need a patched version of rules_go to workaround warnings fixed
# by https://github.com/NixOS/nixpkgs/pull/28029 on NixOS. Revert to
# official release once fix hits Nixpkgs master.
http_archive(
  name = "io_bazel_rules_go",
  strip_prefix = "rules_go-6a2b1f780b475a75a7baae5b441635c566f0ed8a",
  urls = ["https://github.com/mboes/rules_go/archive/6a2b1f780b475a75a7baae5b441635c566f0ed8a.tar.gz"],
)

http_archive(
  name = "com_github_bazelbuild_buildtools",
  strip_prefix = "buildtools-588d90030bc8054b550967aa45a8a8d170deba0b",
  urls = ["https://github.com/bazelbuild/buildtools/archive/588d90030bc8054b550967aa45a8a8d170deba0b.tar.gz"],
)

load(
  "@io_bazel_rules_go//go:def.bzl",
  "go_rules_dependencies",
  "go_register_toolchains",
)

go_rules_dependencies()

# Use host version because none of the SDK's that rules_go knows about
# are compatible with NixOS.
go_register_toolchains(go_version = "host")
