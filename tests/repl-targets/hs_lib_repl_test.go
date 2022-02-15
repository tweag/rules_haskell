package hs_lib_repl_test

import (
	it "github.com/tweag/rules_haskell/tests/integration_testing"
	"testing"
)

func TestMain(m *testing.M) {
	it.TestMain(m, `
-- WORKSPACE --
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
local_repository(
        name = "rules_haskell",
        path = "../rules_haskell",
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")
load("@rules_haskell//tools:os_info.bzl", "os_info")

os_info(name = "os_info")

load("@os_info//:os_info.bzl", "is_windows")

rules_haskell_dependencies()
load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")

haskell_register_ghc_nixpkgs(
    attribute_path = "haskell.compiler.ghc8107",
    repository = "@rules_haskell//nixpkgs:default.nix",
    version = "8.10.7",
)

load("@rules_haskell//haskell:toolchain.bzl", "rules_haskell_toolchains")

rules_haskell_toolchains(version = "8.10.7")

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_python_configure",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)

nixpkgs_cc_configure(
    name = "nixpkgs_config_cc",
    repository = "@rules_haskell//nixpkgs:default.nix",
)

nixpkgs_python_configure(
    repository = "@rules_haskell//nixpkgs:default.nix",
)

nixpkgs_local_repository(
    name = "nixpkgs_default",
    nix_file = "@rules_haskell//nixpkgs:default.nix",
)

nixpkgs_package(
    name = "nixpkgs_zlib",
    attribute_path = "zlib",
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
    linkstatic = 1,
)
""",
    repository = "@nixpkgs_default",
)

http_archive(
    name = "zlib.hs",
    build_file_content = """
load("@os_info//:os_info.bzl", "is_darwin")
load("@rules_cc//cc:defs.bzl", "cc_library")
cc_library(
    name = "zlib",
    srcs = [":z"],
    hdrs = glob(["*.h"]),
    includes = ["."],
    linkstatic = 1,
    visibility = ["//visibility:public"],
)
cc_library(
    name = "z",
    srcs = glob(["*.c"]),
    hdrs = glob(["*.h"]),
    copts = ["-Wno-error=implicit-function-declaration"],
    linkstatic = is_darwin,
)
""",
    sha256 = "c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1",
    strip_prefix = "zlib-1.2.11",
    urls = [
        "https://mirror.bazel.build/zlib.net/zlib-1.2.11.tar.gz",
        "http://zlib.net/zlib-1.2.11.tar.gz",
    ],
)

http_archive(
    name = "alex",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(
    name = "alex",
    srcs = glob(["**"]),
    verbose = False,
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "91aa08c1d3312125fbf4284815189299bbb0be34421ab963b1f2ae06eccc5410",
    strip_prefix = "alex-3.2.6",
    urls = ["http://hackage.haskell.org/package/alex-3.2.6/alex-3.2.6.tar.gz"],
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    components = {
        "alex": [],
        "proto-lens-protoc": [
            "lib",
            "exe",
        ],
    },
    local_snapshot = "@rules_haskell//:stackage_snapshot.yaml",
    packages = [
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
        "c2hs",
        "cabal-doctest",
        "doctest",
        "polysemy",
        "network",
        "language-c",
        "streaming",
        "void",
        "ghc-check",
        "hspec",
        "hspec-core",
        "lens-family-core",
        "data-default-class",
        "profunctors-5.5.2",
        "proto-lens-0.7.0.0",
        "proto-lens-protoc-0.7.0.0",
        "proto-lens-runtime-0.7.0.0",
        "lens-family",
        "safe-exceptions",
        "temporary",
    ],
    setup_deps = {"polysemy": ["cabal-doctest"]},
    stack_snapshot_json = "@rules_haskell//:stackage_snapshot.json" if not is_windows else None,
    tools = [
        "@alex",
    ],
    vendored_packages = {
        "ghc-paths": "@rules_haskell//tools/ghc-paths",
    },
)

register_toolchains(
    ":c2hs-toolchain",
)
-- BUILD.bazel --
load("@rules_haskell//haskell:c2hs.bzl", "c2hs_library", "c2hs_toolchain")
load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_library",
    "haskell_test",
)

load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_toolchain_library",
)

[
    haskell_toolchain_library(name = name)
    for name in [
        "array",
        "base",
    ]
]

genrule(
    name = "codegen",
    outs = [
        "Gen.hs",
    ],
    cmd = """
  echo "module Gen (gen) where" >> $(location :Gen.hs)
  echo "gen :: String" >> $(location :Gen.hs)
  echo "gen = \\"gen\\"" >> $(location :Gen.hs)
""",
)

c2hs_toolchain(
    name = "c2hs-toolchain",
    c2hs = "@stackage-exe//c2hs",
)
c2hs_library(
    name = "chs",
    srcs = ["Chs.chs"],
)

cc_library(
    name = "ourclibrary",
    srcs = [":ourclibrary.c"],
    linkstatic = False,
    visibility = ["//visibility:public"],
)

config_setting(
    name = "nix",
    constraint_values = [
        "@io_tweag_rules_nixpkgs//nixpkgs/constraints:support_nix",
    ],
)

alias(
    name = "zlib",
    # This is a dependency to @stackage-zlib.
    testonly = 0,
    actual = select({
        ":nix": "@zlib.dev//:zlib",
        "//conditions:default": "@zlib.hs//:zlib",
    }),
    visibility = ["//visibility:public"],
)


haskell_library(
    name = "hs-lib",
    srcs = [
        "Foo.hs",
        "Hsc.hsc",
        ":chs",
        ":codegen",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":zlib",
        ":ourclibrary",
        ":array",
        ":base",
    ],
)

-- Chs.chs --
module Chs
  ( baz )
where

baz :: String
baz = "baz"
-- Hsc.hs --
module Hsc
  ( bar )
where

#ifndef _INTERNAL_HSC_DO_NOT_DEFINE_ME
bar :: String
bar = "bar"
#endif
-- Foo.hs --
{-# LANGUAGE ForeignFunctionInterface #-}

module Foo (foo) where

foreign import ccall "c_add_one"
  c_add_one :: Int -> Int

foo :: Int -> Int
foo = (+ 5) . c_add_one
-- ourclibrary.c --
#include <stdint.h>

int32_t c_add_one(int32_t x) {
  return 1 + x;
}
-- Hsc.hsc --
module Hsc
  ( bar )
where

#ifndef _INTERNAL_HSC_DO_NOT_DEFINE_ME
bar :: String
bar = "bar"
#endif
`)
}

func TestHsLibRepl(t *testing.T) {
	out, err := it.BazelOutput(it.Context.BazelBinary, "run", "//:hs-lib@repl", "--", "-ignore-dot-ghci", "-e", "show (foo 10) ++ bar ++ baz ++ gen")
	if err != nil {
		t.Fatal(err)
	}
	it.AssertOutput(t, out, "\"16barbazgen\"\n")
}
