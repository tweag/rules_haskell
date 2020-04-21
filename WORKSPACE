workspace(name = "rules_haskell")

# Subrepositories of rules_haskell

# Some helpers for platform-dependent configuration
load("//tools:os_info.bzl", "os_info")

os_info(name = "os_info")

load("@os_info//:os_info.bzl", "is_linux", "is_nix_shell", "is_windows")

# bazel dependencies
load("//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")

bazel_skylib_workspace()

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "alex",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(
    name = "alex",
    srcs = glob(["**"]),
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "d58e4d708b14ff332a8a8edad4fa8989cb6a9f518a7c6834e96281ac5f8ff232",
    strip_prefix = "alex-3.2.4",
    urls = ["http://hackage.haskell.org/package/alex-3.2.4/alex-3.2.4.tar.gz"],
)

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

http_archive(
    name = "proto-lens-protoc",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(
    name = "proto-lens-protoc",
    srcs = glob(["**"]),
    deps = [
      "@stackage//:base",
      "@stackage//:bytestring",
      "@stackage//:containers",
      "@stackage//:lens-family",
      "@stackage//:proto-lens",
      "@stackage//:proto-lens-protoc",
      "@stackage//:text",
    ],
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "161dcee2aed780f62c01522c86afce61721cf89c0143f157efefb1bd1fa1d164",
    strip_prefix = "proto-lens-protoc-0.5.0.0",
    urls = ["http://hackage.haskell.org/package/proto-lens-protoc-0.5.0.0/proto-lens-protoc-0.5.0.0.tar.gz"],
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    packages = [
        # Core libraries
        "array",
        "base",
        "bytestring",
        "containers",
        "deepseq",
        "directory",
        "filepath",
        "ghc-heap",
        "mtl",
        "process",
        "text",
        "vector",
        # For tests
        "network",
        "language-c",
        "streaming",
        "void",
        "hspec",
        "hspec-core",
        "lens-family-core",
        "data-default-class",
        "proto-lens",
        "proto-lens-protoc",
        "lens-family",
    ],
    snapshot = "lts-14.4",
    tools = [
        "@alex",
        "@happy",
    ],
)

# In a separate repo because not all platforms support zlib.
stack_snapshot(
    name = "stackage-zlib",
    extra_deps = {"zlib": ["@zlib.win//:zlib" if is_windows else "@zlib.dev//:zlib"]},
    packages = ["zlib"],
    snapshot = "lts-13.15",
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_local_repository",
    "nixpkgs_package",
    "nixpkgs_python_configure",
)

http_archive(
    name = "rules_proto",
    sha256 = "73ebe9d15ba42401c785f9d0aeebccd73bd80bf6b8ac78f74996d31f2c0ad7a6",
    strip_prefix = "rules_proto-2c0468366367d7ed97a1f702f9cd7155ab3f73c5",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_proto/archive/2c0468366367d7ed97a1f702f9cd7155ab3f73c5.tar.gz",
        "https://github.com/bazelbuild/rules_proto/archive/2c0468366367d7ed97a1f702f9cd7155ab3f73c5.tar.gz",
    ],
)

load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies", "rules_proto_toolchains")

rules_proto_dependencies()

rules_proto_toolchains()

nixpkgs_local_repository(
    name = "nixpkgs_default",
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
    attribute_path = "haskell.compiler.ghc865",
    compiler_flags = test_compiler_flags,
    haddock_flags = test_haddock_flags,
    locale_archive = "@glibc_locales//:locale-archive",
    repl_ghci_args = test_repl_ghci_args,
    repository = "@nixpkgs_default",
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
)

nixpkgs_cc_configure(
    nix_file = "//nixpkgs:cc-toolchain.nix",
    nix_file_deps = ["//nixpkgs:default.nix"],
    repository = "@nixpkgs_default",
)

nixpkgs_python_configure(repository = "@nixpkgs_default")

nixpkgs_package(
    name = "nixpkgs_zlib",
    attribute_path = "zlib",
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "nixpkgs_lz4",
    attribute_path = "lz4",
    build_file_content = """
package(default_visibility = ["//visibility:public"])
load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
  name = "nixpkgs_lz4",
  srcs = glob(["lib/liblz4.dylib", "lib/liblz4.so*"]),
  includes = ["include"],
)
    """,
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "c2hs",
    attribute_path = "haskell.packages.ghc865.c2hs",
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "doctest",
    attribute_path = "haskell.packages.ghc865.doctest",
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "python3",
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "sphinx",
    attribute_path = "python37Packages.sphinx",
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "graphviz",
    attribute_path = "graphviz",
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "zip",
    attribute_path = "zip",
    repository = "@nixpkgs_default",
)

nixpkgs_package(
    name = "zlib.dev",
    build_file_content = """
load("@rules_cc//cc:defs.bzl", "cc_library")

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
    repository = "@nixpkgs_default",
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
    repository = "@nixpkgs_default",
)

http_archive(
    name = "zlib.win",
    build_file_content = """
load("@rules_cc//cc:defs.bzl", "cc_library")
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
    urls = [
        "https://mirror.bazel.build/zlib.net/zlib-1.2.11.tar.gz",
        "http://zlib.net/zlib-1.2.11.tar.gz",
    ],
)

load("@bazel_tools//tools/build_defs/repo:jvm.bzl", "jvm_maven_import_external")

jvm_maven_import_external(
    name = "org_apache_spark_spark_core_2_10",
    artifact = "org.apache.spark:spark-core_2.10:1.6.0",
    artifact_sha256 = "28aad0602a5eea97e9cfed3a7c5f2934cd5afefdb7f7c1d871bb07985453ea6e",
    licenses = ["notice"],
    server_urls = ["https://repo.maven.apache.org/maven2"],
)

# c2hs rule in its own repository
local_repository(
    name = "c2hs_repo",
    path = "tests/c2hs/repo",
)

load(
    "@rules_haskell//tests/external-haskell-repository:workspace_dummy.bzl",
    "haskell_package_repository_dummy",
)

# dummy repo for the external haskell repo test
haskell_package_repository_dummy(
    name = "haskell_package_repository_dummy",
)

# For Stardoc

nixpkgs_package(
    name = "nixpkgs_nodejs",
    build_file_content = 'exports_files(glob(["nixpkgs_nodejs/**"]))',
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
    repository = "@nixpkgs_default",
)

http_archive(
    name = "build_bazel_rules_nodejs",
    sha256 = "9901bc17138a79135048fb0c107ee7a56e91815ec6594c08cb9a17b80276d62b",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_nodejs/releases/download/0.40.0/rules_nodejs-0.40.0.tar.gz",
        "https://github.com/bazelbuild/rules_nodejs/releases/download/0.40.0/rules_nodejs-0.40.0.tar.gz",
    ],
)

load("@build_bazel_rules_nodejs//:defs.bzl", "node_repositories")

node_repositories(
    vendored_node = "@nixpkgs_nodejs",
)

http_archive(
    name = "io_bazel_rules_sass",
    sha256 = "d5e0c0d16fb52f3dcce5bd7830d92d4813eb01bac0211119e74ec9e65eaf3b86",
    strip_prefix = "rules_sass-1.23.3",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_sass/archive/1.23.3.tar.gz",
        "https://github.com/bazelbuild/rules_sass/archive/1.23.3.tar.gz",
    ],
)

load("@io_bazel_rules_sass//:package.bzl", "rules_sass_dependencies")

rules_sass_dependencies()

load("@io_bazel_rules_sass//:defs.bzl", "sass_repositories")

sass_repositories()

http_archive(
    name = "io_bazel_stardoc",
    sha256 = "6d07d18c15abb0f6d393adbd6075cd661a2219faab56a9517741f0fc755f6f3c",
    strip_prefix = "stardoc-0.4.0",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/stardoc/archive/0.4.0.tar.gz",
        "https://github.com/bazelbuild/stardoc/archive/0.4.0.tar.gz",
    ],
)

load("@io_bazel_stardoc//:setup.bzl", "stardoc_repositories")

stardoc_repositories()

load(
    "@rules_haskell//docs/pandoc:pandoc.bzl",
    "import_pandoc_bindists",
    "nixpkgs_pandoc_configure",
)

nixpkgs_pandoc_configure(repository = "@nixpkgs_default")

import_pandoc_bindists()

register_toolchains(
    "@rules_haskell//docs/pandoc:nixpkgs",
    "@rules_haskell//docs/pandoc:linux",
    "@rules_haskell//docs/pandoc:macos",
)

# For buildifier

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "b9aa86ec08a292b97ec4591cf578e020b35f98e12173bbd4a921f84f583aebd9",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.20.2/rules_go-v0.20.2.tar.gz",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.20.2/rules_go-v0.20.2.tar.gz",
    ],
)

http_archive(
    name = "com_github_bazelbuild_buildtools",
    sha256 = "f3ef44916e6be705ae862c0520bac6834dd2ff1d4ac7e5abc61fe9f12ce7a865",
    strip_prefix = "buildtools-0.29.0",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/buildtools/archive/0.29.0.tar.gz",
        "https://github.com/bazelbuild/buildtools/archive/0.29.0.tar.gz",
    ],
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

# If in nix-shell, use the Go SDK provided by Nix.
# Otherwise, ask Bazel to download a Go SDK.
go_register_toolchains(go_version = "host") if is_nix_shell else go_register_toolchains()

load("@com_github_bazelbuild_buildtools//buildifier:deps.bzl", "buildifier_dependencies")

buildifier_dependencies()

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
