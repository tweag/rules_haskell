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
    attribute_path = "haskell.packages.ghc843.proto-lens-protoc"
)

nixpkgs_package(
    name = "doctest",
    repository = "//nixpkgs:default.nix",
    attribute_path = "haskell.packages.ghc843.doctest",
)

nixpkgs_package(
    name = "c2hs",
    repository = "//nixpkgs:default.nix",
    attribute_path = "haskell.packages.ghc843.c2hs",
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

http_archive(
    name = "io_bazel_rules_go",
    strip_prefix = "rules_go-0.15.1",
    urls = ["https://github.com/bazelbuild/rules_go/archive/0.15.1.tar.gz"],
    sha256 = "168330e27e30dc1344cc49dfe7383e4566f67d555365ebd494d1ce8f48812f3e",
)

http_archive(
    name = "com_github_bazelbuild_buildtools",
    strip_prefix = "buildtools-0.17.2",
    urls = ["https://github.com/bazelbuild/buildtools/archive/0.17.2.tar.gz"],
    sha256 = "5186fcecb2762a1a3f2978b01dd5825aad91369c2646bc5b42fe1be774aff351",
)

load(
    "@io_bazel_rules_go//go:def.bzl",
    "go_wrap_sdk",
    "go_rules_dependencies",
    "go_register_toolchains",
)

nixpkgs_package(
    name = "go",
    attribute_path = "go",
    repository = "//nixpkgs:default.nix",
)

# Use the go distribution from nixpkgs instead of letting rules_go
# download some arbitrary binary distribution. This fixes buildifier
# on both NixOS and Darwin.
go_wrap_sdk(
    name = "go_sdk",
    # root_file should point to a file in the go distribution folder,
    # which is `share/go` in the nixpkgs package.
    root_file = "@go//:share/go/README.md",
)

go_rules_dependencies()

go_register_toolchains()
