workspace(name = "io_tweag_rules_haskell")


http_archive(
  name = "io_tweag_rules_nixpkgs",
  strip_prefix = "rules_nixpkgs-0.1",
  urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.1.tar.gz"],
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_package")

nixpkgs_package(
  name = "ghc",
  attribute_path = "haskell.compiler.ghc822",
  build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
  name = "bin",
  srcs = glob(["nix/bin/*"]),
)

filegroup(
  name = "lib",
  srcs = glob(["nix/lib/**/*.so"]),
)

cc_library(
  name = "threaded-rts",
  srcs = glob(["nix/lib/ghc-*/rts/libHSrts_thr-ghc*.so"]),
  hdrs = glob(["nix/lib/ghc-*/include/**/*.h"]),
  strip_include_prefix = glob(["nix/lib/ghc-*/include"], exclude_directories=0)[0],
)
""",
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
