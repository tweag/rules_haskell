workspace(name = "io_tweag_rules_haskell")

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

http_archive(
  name = "io_tweag_rules_nixpkgs",
  strip_prefix = "rules_nixpkgs-0.2.2",
  urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.2.2.tar.gz"],
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
  revision = "7c3dc2f53fc837be79426f11c9133f73d15a05c4",
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
  nix_file = "//tests:protoc_gen_haskell.nix",
)

nixpkgs_package(
  name = "doctest",
  repository = "@nixpkgs",
  attribute_path = "haskell.packages.ghc822.doctest",
  build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
  name = "bin",
  srcs = ["bin/doctest"],
)
  """
)

register_toolchains(
  "//tests:ghc",
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
package(default_visibility = ["//visibility:public"])

filegroup (
  name = "include",
  srcs = glob(["include/*.h"]),
  testonly = 1,
)
""",
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
