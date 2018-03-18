workspace(name = "io_tweag_rules_haskell")

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

http_archive(
  name = "io_tweag_rules_nixpkgs",
  strip_prefix = "rules_nixpkgs-0.2",
  urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.2.tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_package")

nixpkgs_package(
  name = "ghc",
  attribute_path = "haskell.compiler.ghc822",
  build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
  name = "bin",
  srcs = glob(["bin/*"]),
)

cc_library(
  name = "threaded-rts",
  srcs = glob(["lib/ghc-*/rts/libHSrts_thr-ghc*.so"]),
  hdrs = glob(["lib/ghc-*/include/**/*.h"]),
  strip_include_prefix = glob(["lib/ghc-*/include"], exclude_directories=0)[0],
)
""",
)

register_toolchains("//tests:ghc")

nixpkgs_package(name = "zlib", build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup (
  name = "lib",
  srcs = glob([
    "lib/*.so",
    "lib/*.so.*",
  ]),
  testonly = 1,
)
""",
)

nixpkgs_package(name = "zlib.dev", build_file_content = """
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
