workspace(name = "rules_haskell")

# Subrepositories of rules_haskell

# various examples
local_repository(
    name = "rules_haskell_examples",
    path = "examples",
)

# code for the tutorial
local_repository(
    name = "rules_haskell_tutorial",
    path = "tutorial",
)

# hazel, a way to generate bazel libraries from [st/h]ackage
local_repository(
    name = "ai_formation_hazel",
    path = "hazel",
)

# Some helpers for platform-dependent configuration
load("//tools:os_info.bzl", "os_info")

os_info(name = "os_info")

load("@os_info//:os_info.bzl", "is_linux", "is_windows")

# bazel dependencies
load("//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "happy",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(name = "happy", srcs = glob(["**"]), visibility = ["//visibility:public"])
    """,
    sha256 = "22eb606c97105b396e1c7dc27e120ca02025a87f3e44d2ea52be6a653a52caed",
    strip_prefix = "happy-1.19.10",
    urls = ["http://hackage.haskell.org/package/happy-1.19.10/happy-1.19.10.tar.gz"],
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    packages = [
        # Core libraries
        "array",
        "base",
        "directory",
        "filepath",
        "ghc-heap",
        "process",
        # For tests
        "streaming",
        "void",
        "hspec",
        "hspec-core",
        "lens-family-core",
        "data-default-class",
        "lens-labels",
        "proto-lens",
        "lens-family",
    ],
    snapshot = "lts-13.15",
    tools = ["@happy"],
)

# In a separate repo because not all platforms support zlib.
stack_snapshot(
    name = "stackage-zlib",
    packages = ["zlib"],
    snapshot = "lts-13.15",
    deps = {"zlib": ["@zlib.win//:zlib" if is_windows else "@zlib.dev//:zlib"]},
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)

nixpkgs_package(
    name = "ghc",
    repository = "@nixpkgs",
)

http_archive(
    name = "com_google_protobuf",
    sha256 = "03d2e5ef101aee4c2f6ddcf145d2a04926b9c19e7086944df3842b1b8502b783",
    strip_prefix = "protobuf-3.8.0",
    urls = ["https://github.com/google/protobuf/archive/v3.8.0.tar.gz"],
)

load("@com_google_protobuf//:protobuf_deps.bzl", "protobuf_deps")

protobuf_deps()  # configure and install zlib for protobuf

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//nixpkgs:default.nix",
)

test_compiler_flags = [
    "-XStandaloneDeriving",  # Flag used at compile time
    "-threaded",  # Flag used at link time

    # Used by `tests/repl-flags`
    "-DTESTS_TOOLCHAIN_COMPILER_FLAGS",
    # this is the default, so it does not harm other tests
    "-XNoOverloadedStrings",
]

test_haddock_flags = ["-U"]

test_repl_ghci_args = [
    # The repl test will need this flag, but set by the local
    # `repl_ghci_args`.
    "-UTESTS_TOOLCHAIN_REPL_FLAGS",
    # The repl test will need OverloadedString
    "-XOverloadedStrings",
]

load(
    "@rules_haskell//:constants.bzl",
    "test_ghc_version",
)
load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)

haskell_register_ghc_nixpkgs(
    attribute_path = "ghc",
    compiler_flags = test_compiler_flags,
    haddock_flags = test_haddock_flags,
    locale_archive = "@glibc_locales//:locale-archive",
    repl_ghci_args = test_repl_ghci_args,
    repository = "@nixpkgs",
    version = test_ghc_version,
)

load(
    "@rules_haskell//haskell:ghc_bindist.bzl",
    "haskell_register_ghc_bindists",
)

haskell_register_ghc_bindists(
    compiler_flags = test_compiler_flags,
    version = test_ghc_version,
)

register_toolchains(
    "//tests:c2hs-toolchain",
    "//tests:doctest-toolchain",
    "//tests:protobuf-toolchain",
    # XXX: see .bazelrc for discussion, the python toolchain
    # work in postponed to future bazel version
    # "//tests:python_toolchain",
)

nixpkgs_cc_configure(
    nix_file = "//nixpkgs:cc-toolchain.nix",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "nixpkgs_zlib",
    attribute_path = "zlib",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "nixpkgs_lz4",
    attribute_path = "lz4",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

cc_library(
  name = "nixpkgs_lz4",
  srcs = glob(["lib/liblz4.dylib", "lib/liblz4.so*"]),
  includes = ["include"],
)
    """,
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "c2hs",
    attribute_path = "haskellPackages.c2hs",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "doctest",
    attribute_path = "haskellPackages.doctest",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "proto-lens-protoc",
    attribute_path = "haskellPackages.proto-lens-protoc",
    repository = "@nixpkgs",
)

#nixpkgs_package(
#    name = "python3",
#    attribute_path = "python3",
#    repository = "@nixpkgs",
#)

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
filegroup(
    name = "include",
    srcs = glob(["include/*.h"]),
    visibility = ["//visibility:public"],
)

cc_library(
    name = "zlib",
    srcs = ["@nixpkgs_zlib//:lib"],
    hdrs = [":include"],
    strip_include_prefix = "include",
    visibility = ["//visibility:public"],
)
""",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "glibc_locales",
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

http_archive(
    name = "zlib.win",
    build_file_content = """
cc_library(
    name = "zlib",
    # Import `:z` as `srcs` to enforce the library name `libz.so`. Otherwise,
    # Bazel would mangle the library name and e.g. Cabal wouldn't recognize it.
    srcs = [":z"],
    hdrs = glob(["*.h"]),
    includes = ["."],
    visibility = ["//visibility:public"],
)
cc_library(name = "z", srcs = glob(["*.c"]), hdrs = glob(["*.h"]))
""",
    sha256 = "c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1",
    strip_prefix = "zlib-1.2.11",
    urls = ["http://zlib.net/zlib-1.2.11.tar.gz"],
)

load("@bazel_tools//tools/build_defs/repo:jvm.bzl", "jvm_maven_import_external")

jvm_maven_import_external(
    name = "org_apache_spark_spark_core_2_10",
    artifact = "org.apache.spark:spark-core_2.10:1.6.0",
    artifact_sha256 = "28aad0602a5eea97e9cfed3a7c5f2934cd5afefdb7f7c1d871bb07985453ea6e",
    licenses = ["notice"],
    server_urls = ["http://central.maven.org/maven2"],
)

# c2hs rule in its own repository
local_repository(
    name = "c2hs_repo",
    path = "tests/c2hs/repo",
)

# python toolchain
nixpkgs_package(
    name = "python3",
    attribute_path = "python3",
    repository = "@nixpkgs",
)

load(
    "@rules_haskell//tests/external-haskell-repository:workspace_dummy.bzl",
    "haskell_package_repository_dummy",
)

# dummy repo for the external haskell repo test (hazel)
haskell_package_repository_dummy(
    name = "haskell_package_repository_dummy",
)

# For Skydoc

nixpkgs_package(
    name = "nixpkgs_nodejs",
    # XXX Indirection derivation to make all of NodeJS rooted in
    # a single directory. We shouldn't need this, but it's
    # a workaround for
    # https://github.com/bazelbuild/bazel/issues/2927.
    nix_file_content = """
    with import <nixpkgs> { config = {}; overlays = []; };
    runCommand "nodejs-rules_haskell" { buildInputs = [ nodejs ]; } ''
      mkdir -p $out/nixpkgs_nodejs
      cd $out/nixpkgs_nodejs
      for i in ${nodejs}/*; do ln -s $i; done
      ''
    """,
    nixopts = [
        "--option",
        "sandbox",
        "false",
    ],
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
    sha256 = "c2d66a0cc7e25d857e480409a8004fdf09072a1bd564d6824441ab2f96448eea",
    strip_prefix = "skydoc-0.3.0",
    urls = ["https://github.com/bazelbuild/skydoc/archive/0.3.0.tar.gz"],
)

load("@io_bazel_skydoc//:setup.bzl", "skydoc_repositories")

skydoc_repositories()

# For buildifier

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "1ad10f384053ae50c050fdec7d595d12427c82c0a27c58f8554deb4437216892",
    strip_prefix = "rules_go-0.18.7",
    urls = ["https://github.com/bazelbuild/rules_go/archive/0.18.7.tar.gz"],
)

http_archive(
    name = "com_github_bazelbuild_buildtools",
    sha256 = "5ec71602e9b458b01717fab1d37492154c1c12ea83f881c745dbd88e9b2098d8",
    strip_prefix = "buildtools-0.28.0",
    urls = ["https://github.com/bazelbuild/buildtools/archive/0.28.0.tar.gz"],
)

# A repository that generates the Go SDK imports, see ./tools/go_sdk/README
local_repository(
    name = "go_sdk_repo",
    path = "tools/go_sdk",
)

load(
    "@io_bazel_rules_go//go:deps.bzl",
    "go_register_toolchains",
    "go_rules_dependencies",
)

go_rules_dependencies()

# If Windows, ask Bazel to download a Go SDK. Otherwise use the nix-shell
# provided GO SDK.
go_register_toolchains() if is_windows else go_register_toolchains(go_version = "host")

load("@com_github_bazelbuild_buildtools//buildifier:deps.bzl", "buildifier_dependencies")

buildifier_dependencies()

# Hazel

load("@ai_formation_hazel//:workspace.bzl", "hazel_setup")

hazel_setup()

# For profiling
# Required to make use of `bazel build --profile`.

# Dummy target //external:python_headers.
# See https://github.com/protocolbuffers/protobuf/blob/d9ccd0c0e6bbda9bf4476088eeb46b02d7dcd327/util/python/BUILD
bind(
    name = "python_headers",
    actual = "@com_google_protobuf//util/python:python_headers",
)

# For persistent worker (tools/worker)
load("//tools:repositories.bzl", "rules_haskell_worker_dependencies")

rules_haskell_worker_dependencies()
