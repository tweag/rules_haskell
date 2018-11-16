workspace(name = "io_tweag_rules_haskell")

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-0.4.0",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.4.0.tar.gz"],
    sha256 = "a4aefad582fcc22301b8696df7d6f55ac3183593f7efa693583f3a5d79a0aa58",
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_git_repository",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)
load("@io_tweag_rules_haskell//haskell:nix.bzl",
     "haskell_nixpkgs_packages",
     "haskell_nixpkgs_package",
     "haskell_nixpkgs_packageset")

haskell_nixpkgs_package(
    name = "ghc",
    # rules_nixpkgs assumes we want to read from `<nixpkgs>` implicitly
    # if `repository` is not set, but our nix_file uses `./nixpkgs/`.
    # TODO(Profpatsch)
    repositories = { "nixpkgs": "//nixpkgs:NOTUSED" },
    nix_file = "//tests:ghc.nix",
    attribute_path = "haskellPackages.ghc",
    build_file = "//haskell:ghc.BUILD",
)

http_archive(
    name = "com_google_protobuf",
    sha256 = "cef7f1b5a7c5fba672bec2a319246e8feba471f04dcebfe362d55930ee7c1c30",
    strip_prefix = "protobuf-3.5.0",
    urls = ["https://github.com/google/protobuf/archive/v3.5.0.zip"],
)

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//nixpkgs:default.nix",
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

haskell_nixpkgs_packageset(
    name = "hackage-packages",
    repositories = { "nixpkgs": "@nixpkgs" },
    nix_file = "//tests:ghc.nix",
    base_attribute_path = "haskellPackages",
)
load("@hackage-packages//:packages.bzl", "import_packages")
import_packages(name = "hackage")

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
