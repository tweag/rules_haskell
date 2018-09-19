workspace(name = "io_tweag_rules_haskell")

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-0.2.3",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.2.3.tar.gz"],
    sha256 = "2647bc9d5476fba95d9b4cc300be1ba9ad353e4e33bee01e041886aa4f4b554a",
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_git_repository",
    "nixpkgs_package",
)

nixpkgs_package(
    name = "ghc",
    # rules_nixpkgs assumes we want to read from `<nixpkgs>` implicitly
    # if `repository` is not set, but our nix_file uses `./nixpkgs/`.
    # TODO(Profpatsch)
    repository = "//nixpkgs:NOTUSED",
    nix_file = "//tests:ghc.nix",
    attribute_path = "ghc",
    build_file = "//haskell:ghc.BUILD",
)

http_archive(
    name = "com_google_protobuf",
    sha256 = "cef7f1b5a7c5fba672bec2a319246e8feba471f04dcebfe362d55930ee7c1c30",
    strip_prefix = "protobuf-3.5.0",
    urls = ["https://github.com/google/protobuf/archive/v3.5.0.zip"],
)

nixpkgs_package(
    name = "protoc_gen_haskell",
    # this is a trick to set <nixpkgs> to reference a nix file
    # TODO(Profpatsch) document & fix upstream
    repository = "//nixpkgs:default.nix",
    attribute_path = "haskell.packages.ghc844.proto-lens-protoc"
)

nixpkgs_package(
    name = "doctest",
    repository = "//nixpkgs:default.nix",
    attribute_path = "haskell.packages.ghc844.doctest",
)

nixpkgs_package(
    name = "c2hs",
    repository = "//nixpkgs:default.nix",
    attribute_path = "haskell.packages.ghc844.c2hs",
)

register_toolchains(
    "//tests:ghc",
    "//tests:doctest-toolchain",
    "//tests:protobuf-toolchain",
)

nixpkgs_package(
    name = "zlib",
    repository = "//nixpkgs:default.nix",
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
    repository = "//nixpkgs:default.nix",
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
    repository = "//nixpkgs:default.nix",
    attribute_path = "glibcLocales",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
    name = "locale-archive",
    srcs = ["lib/locale/locale-archive"],
)
"""
)

nixpkgs_package(
    name = "template-haskell",
    repository = "@nixpkgs",
    nix_file = "//tests:ghc.nix",
    attribute_path = "ghc",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
  name = "import_dirs",
  srcs = ["lib/ghc-8.2.2/template-haskell-2.12.0.0"],
)

filegroup(
  name = "package_conf",
  srcs = ["lib/ghc-8.2.2/package.conf.d/template-haskell-2.12.0.0.conf"],
)

"""
)
nixpkgs_package(
    name = "void",
    repository = "@nixpkgs",
    nix_file = "//tests:ghc.nix",
    attribute_path = "ghc",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
  name = "import_dirs",
  srcs = ["lib/ghc-8.2.2/void-0.7.2"],
)

filegroup(
  name = "package_conf",
  srcs = ["lib/ghc-8.2.2/package.conf.d/void-0.7.2-BaCvWtelk6X2yBHg8R7RQm.conf"],
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
        sha256 = "14536292b14b5d36d1d72ae68ee7384a51e304fa35a3c4e4db0f4590394f36ad",
)
load("@io_bazel_rules_sass//sass:sass.bzl", "sass_repositories")
sass_repositories()

http_archive(
        name = "io_bazel_skydoc",
        strip_prefix = "skydoc-b374449408e759e32e010fa6a20585fe9fabd523",
        urls = ["https://github.com/mrkkrp/skydoc/archive/b374449408e759e32e010fa6a20585fe9fabd523.tar.gz"],
        sha256 = "12b96f74de7a6002de69a92959e476bd8c9ed95cb969354ee1af750f9961203b",
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
