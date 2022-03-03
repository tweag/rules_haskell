package hs_bin_repl_test

import (
	bt "github.com/bazelbuild/rules_go/go/tools/bazel_testing"
	it "github.com/tweag/rules_haskell/tests/integration_testing"
	"testing"
)

var testcase = `
-- WORKSPACE --
local_repository(
        name = "rules_haskell",
        path = "../rules_haskell",
)
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

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
)

nixpkgs_cc_configure(
    name = "nixpkgs_config_cc",
    repository = "@rules_haskell//nixpkgs:default.nix",
)

nixpkgs_python_configure(
    repository = "@rules_haskell//nixpkgs:default.nix",
)

-- BUILD.bazel --
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
        "base",
    ]
]

haskell_library(
    name = "QuuxLib",
    srcs = ["QuuxLib.hs"],
    deps = [":base"],
)

haskell_test(
    name = "hs-bin",
    srcs = ["Quux.hs"],
    visibility = ["//visibility:public"],
    deps = [
        ":QuuxLib",
        ":base",
    ],
)

-- Quux.hs --
module Main (main) where

import QuuxLib (message)

main :: IO ()
main = putStrLn message
-- QuuxLib.hs --
module QuuxLib (message) where

message :: String
message = "Hello GHCi!"
`

func TestMain(m *testing.M) {
	it.TestMain(m, bt.Args{Main: testcase})
}

func TestHsBinRepl(t *testing.T) {
	out, err := it.BazelOutput(it.Context.BazelBinary, "run", "//:hs-bin@repl", "--", "-ignore-dot-ghci", "-e", ":main")
	if err != nil {
		t.Fatal(err)
	}
	it.AssertOutput(t, out, "Hello GHCi!\n")
}
