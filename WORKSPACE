workspace(name = "io_tweag_rules_haskell")

local_repository(
  name = "examples",
  path = "examples",
)

http_archive(
  name = "io_tweag_rules_nixpkgs",
  strip_prefix = "rules_nixpkgs-0.1",
  urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.1.tar.gz"],
)

http_archive(
  name = "bazel_skylib",
  strip_prefix = "bazel-skylib-0.2.0",
  urls = ["https://github.com/bazelbuild/bazel-skylib/archive/0.2.0.tar.gz"]
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_package")

nixpkgs_package(
  name = "ghc",
  attribute_path = "haskell.compiler.ghc822",
)

# For tests

register_toolchains("//tests:toolchain")

nixpkgs_package(name = "zlib", build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup (
  name = "lib",
  srcs = glob(["nix/lib/*.so"]),
  testonly = 1,
)
""",
)

nixpkgs_package(name = "zlib.dev", build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup (
  name = "include",
  srcs = glob(["nix/include/*.h"]),
  testonly = 1,
)
""",
)

maven_jar(
  name = "org_apache_spark_spark_core_2_10",
  artifact = "org.apache.spark:spark-core_2.10:1.6.0",
)
