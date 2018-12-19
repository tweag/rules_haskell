workspace(name = "io_tweag_rules_haskell")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")

haskell_repositories()

http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = "e08bfff0e3413cae8549df72e3fce36f7b0e2369e864dfe41d3307ef100500f8",
    strip_prefix = "rules_nixpkgs-0.4.1",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.4.1.tar.gz"],
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)
load(
    "@io_tweag_rules_haskell//haskell:nix.bzl",
    "haskell_nixpkgs_package",
    "haskell_nixpkgs_packageset",
)

haskell_nixpkgs_package(
    name = "ghc",
    attribute_path = "haskellPackages.ghc",
    build_file = "//haskell:ghc.BUILD",
    nix_file = "//tests:ghc.nix",
    # rules_nixpkgs assumes we want to read from `<nixpkgs>` implicitly
    # if `repository` is not set, but our nix_file uses `./nixpkgs/`.
    # TODO(Profpatsch)
    repositories = {"nixpkgs": "//nixpkgs:NOTUSED"},
)

http_archive(
    name = "com_google_protobuf",
    sha256 = "d625beb4a43304409429a0466bb4fb44c89f7e7d90aeced972b8a61dbe92c80b",
    strip_prefix = "protobuf-7b28271a61a3da0a37f6fda399b0c4c86464e5b3",
    urls = ["https://github.com/google/protobuf/archive/7b28271a61a3da0a37f6fda399b0c4c86464e5b3.zip"],  # 2018-11-16
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

nixpkgs_cc_configure(
    nix_file = "//nixpkgs:cc-toolchain.nix",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "zlib",
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
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "zlib.dev",
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
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "glib_locales",
    attribute_path = "glibcLocales",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
    name = "locale-archive",
    srcs = ["lib/locale/locale-archive"],
)
""",
    repository = "@nixpkgs",
)

haskell_nixpkgs_packageset(
    name = "hackage-packages",
    base_attribute_path = "haskellPackages",
    nix_file = "//tests:ghc.nix",
    repositories = {"nixpkgs": "@nixpkgs"},
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
    sha256 = "14536292b14b5d36d1d72ae68ee7384a51e304fa35a3c4e4db0f4590394f36ad",
    strip_prefix = "rules_sass-0.0.3",
    urls = ["https://github.com/bazelbuild/rules_sass/archive/0.0.3.tar.gz"],
)

load("@io_bazel_rules_sass//sass:sass.bzl", "sass_repositories")

sass_repositories()

http_archive(
    name = "io_bazel_skydoc",
    sha256 = "12a82b494a40c4ef96230bc66aeff654420dd39a537eb3064ff18ce1838f1fb7",
    strip_prefix = "skydoc-9bbdf62c03b5c3fed231604f78d3976f47753d79",
    urls = ["https://github.com/mrkkrp/skydoc/archive/9bbdf62c03b5c3fed231604f78d3976f47753d79.tar.gz"],
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

bazelbuild_buildtools_rev = "4a7914a1466ff7388c934bfcd43a3852928536f6"

http_archive(
    name = "com_github_bazelbuild_buildtools",
    sha256 = "45775c7bb7ee7656e9df4ca4278f977c8e4e260aff755734734c19321e14bc84",
    strip_prefix = "buildtools-%s" % bazelbuild_buildtools_rev,
    url = "https://github.com/bazelbuild/buildtools/archive/%s.zip" % bazelbuild_buildtools_rev,
)

load(
    "@io_bazel_rules_go//go:def.bzl",
    "go_register_toolchains",
    "go_rules_dependencies",
)
load("@com_github_bazelbuild_buildtools//buildifier:deps.bzl", "buildifier_dependencies")

go_rules_dependencies()

# Use host version because none of the SDK's that rules_go knows about
# are compatible with NixOS.
go_register_toolchains(go_version = "host")

buildifier_dependencies()
