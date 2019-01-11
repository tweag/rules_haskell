workspace(name = "io_tweag_rules_haskell")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")

haskell_repositories()

http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = "4f924d839d3a5896e3e22ba1282c686b2c078fd4c98d564ec427ac83ec66302d",
    strip_prefix = "rules_nixpkgs-0.5.1",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.5.1.tar.gz"],
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
    sha256 = "73fdad358857e120fd0fa19e071a96e15c0f23bb25f85d3f7009abfd4f264a2a",
    strip_prefix = "protobuf-3.6.1.3",
    urls = ["https://github.com/google/protobuf/archive/v3.6.1.3.tar.gz"],
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
    name = "sphinx",
    attribute_path = "python36Packages.sphinx",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "graphviz",
    attribute_path = "graphviz",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "zip",
    attribute_path = "zip",
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
    nixopts = [
        "-j",
        "1",
    ],
    repositories = {"nixpkgs": "@nixpkgs"},
)

load("@hackage-packages//:packages.bzl", "import_packages")

import_packages(name = "hackage")

# zlib as a Haskell library

http_archive(
    name = "haskell_zlib",
    build_file = "//tests:BUILD.zlib",
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

nixpkgs_package(
    name = "nixpkgs_nodejs",
    # XXX Indirection derivation to make all of NodeJS rooted in
    # a single directory. We shouldn't need this, but it's
    # a workaround for
    # https://github.com/bazelbuild/bazel/issues/2927.
    nix_file_content = """
    with import <nixpkgs> {};
    runCommand "nodejs-rules_haskell" { buildInputs = [ nodejs ]; } ''
      mkdir -p $out/nixpkgs_nodejs
      cd $out/nixpkgs_nodejs
      for i in ${nodejs}/*; do ln -s $i; done
      ''
    """,
    repository = "@nixpkgs",
)

http_archive(
    name = "build_bazel_rules_nodejs",
    sha256 = "f79f605a920145216e64991d6eff4e23babc48810a9efd63a31744bb6637b01e",
    strip_prefix = "rules_nodejs-b4dad57d2ecc63d74db1f5523593639a635e447d",
    # Tip of https://github.com/bazelbuild/rules_nodejs/pull/471.
    urls = ["https://github.com/mboes/rules_nodejs/archive/b4dad57d2ecc63d74db1f5523593639a635e447d.tar.gz"],
)

http_archive(
    name = "io_bazel_rules_sass",
    sha256 = "1e135452dc627f52eab39a50f4d5b8d13e8ed66cba2e6da56ac4cbdbd776536c",
    strip_prefix = "rules_sass-1.15.2",
    urls = ["https://github.com/bazelbuild/rules_sass/archive/1.15.2.tar.gz"],
)

load("@io_bazel_rules_sass//:package.bzl", "rules_sass_dependencies")

rules_sass_dependencies()

load("@io_bazel_rules_sass//:defs.bzl", "sass_repositories")

sass_repositories()

load("@build_bazel_rules_nodejs//:defs.bzl", "node_repositories")

node_repositories(
    vendored_node = "@nixpkgs_nodejs",
)

http_archive(
    name = "io_bazel_skydoc",
    sha256 = "19eb6c162075707df5703c274d3348127625873dbfa5ff83b1ef4b8f5dbaa449",
    strip_prefix = "skydoc-0.2.0",
    urls = ["https://github.com/bazelbuild/skydoc/archive/0.2.0.tar.gz"],
)

load("@io_bazel_skydoc//:setup.bzl", "skydoc_repositories")

skydoc_repositories()

# For buildifier

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "8be57ff66da79d9e4bd434c860dce589195b9101b2c187d144014bbca23b5166",
    strip_prefix = "rules_go-0.16.3",
    urls = ["https://github.com/bazelbuild/rules_go/archive/0.16.3.tar.gz"],
)

http_archive(
    name = "com_github_bazelbuild_buildtools",
    sha256 = "c730536b703b10294675743579afa78055d3feda92e8cb03d2fb76ad97396770",
    strip_prefix = "buildtools-0.20.0",
    urls = ["https://github.com/bazelbuild/buildtools/archive/0.20.0.tar.gz"],
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
