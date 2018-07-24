workspace(name = "io_tweag_rules_haskell")

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

http_archive(
  name = "io_tweag_rules_nixpkgs",
  strip_prefix = "rules_nixpkgs-0.2.3",
  urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.2.3.tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
  "nixpkgs_git_repository",
  "nixpkgs_package",
)

nixpkgs_git_repository(
  name = "nixpkgs",
  # To make protobuf support work we need packages such as
  # lens-labels_0_2_0_0 to be available in nixpkgs. This means we need to
  # use a version of nixpkgs that is newer than 18.03.

  # You need to be in an environment that is on the same commit
  # as the one below. Use `nix-shell shell.nix`.

  # Keep this value in sync with `nixpkgs.nix`
  revision = "9a787af6bc75a19ac9f02077ade58ddc248e674a",
  # TODO Using a fork with Bazel v0.15. Switch to mainline once
  # https://github.com/NixOS/nixpkgs/pull/42735 merged.
  remote = "https://github.com/mboes/nixpkgs",
)

nixpkgs_package(
  name = "ghc",
  repository = "@nixpkgs",
  nix_file = "//tests:ghc.nix",
  build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
  name = "bin",
  srcs = glob(["bin/*"]),
)

cc_library(
  name = "threaded-rts",
  srcs = select({
      "@bazel_tools//src/conditions:darwin":
          glob([
            "lib/ghc-*/rts/libHSrts_thr-ghc*.dylib",
            "lib/ghc-*/rts/libffi.dylib",
          ]),
      "//conditions:default":
          glob([
            "lib/ghc-*/rts/libHSrts_thr-ghc*.so",
            "lib/ghc-*/rts/libffi.so.6",
          ]),
  }),
  hdrs = glob(["lib/ghc-*/include/**/*.h"]),
  strip_include_prefix = glob(["lib/ghc-*/include"], exclude_directories=0)[0],
)
""",
)

http_archive(
  name = "com_google_protobuf",
  sha256 = "cef7f1b5a7c5fba672bec2a319246e8feba471f04dcebfe362d55930ee7c1c30",
  strip_prefix = "protobuf-3.5.0",
  urls = ["https://github.com/google/protobuf/archive/v3.5.0.zip"],
)

nixpkgs_package(
  name = "protoc_gen_haskell",
  repository = "@nixpkgs",
  attribute_path = "haskell.packages.ghc822.proto-lens-protoc"
)

nixpkgs_package(
  name = "doctest",
  repository = "@nixpkgs",
  attribute_path = "haskell.packages.ghc822.doctest",
)

nixpkgs_package(
  name = "c2hs",
  repository = "@nixpkgs",
  attribute_path = "haskell.packages.ghc822.c2hs",
)

register_toolchains(
  "//tests:ghc",
  "//tests:doctest-toolchain",
  "//tests:protobuf-toolchain",
)

nixpkgs_package(
  name = "zlib",
  repository = "@nixpkgs",
  build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup (
  name = "lib",
  srcs = glob([
    "lib/*.so",
    "lib/*.so.*",
    "lib/*.dylib",
  ]),
  testonly = 1,
)
""",
)

nixpkgs_package(
  name = "zlib.dev",
  repository = "@nixpkgs",
  build_file_content = """
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_cc_import")
package(default_visibility = ["//visibility:public"])

filegroup (
  name = "include",
  srcs = glob(["include/*.h"]),
  testonly = 1,
)

haskell_cc_import(
    name = "zlib",
    shared_library = "@zlib//:lib",
    hdrs = [":include"],
    testonly = 1,
    strip_include_prefix = "include",
)
""",
)

nixpkgs_package(
  name = "glib_locales",
  repository = "@nixpkgs",
  attribute_path = "glibcLocales",
  build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
  name = "locale-archive",
  srcs = ["lib/locale/locale-archive"],
)
"""
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

# c2hs rule in its own repository
local_repository(
    name = "c2hs_repo",
    path = "tests/c2hs/repo",
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
