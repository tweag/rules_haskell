workspace(name = "io_tweag_rules_haskell")

local_repository(
  name = "examples",
  path = "examples"
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
  attribute_path = "haskell.packages.ghc822.ghc"
)

# For tests

nixpkgs_package(name = "zlib", build_file_content = """
filegroup (
  name = "lib",
  srcs = glob(["nix/lib/**/*.so"]),
  visibility = ["//visibility:public"],
  testonly = 1,
)""",
)

register_toolchains("//tests:toolchain")
