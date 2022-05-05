package haskell_module_repl_test

import (
	bt "github.com/bazelbuild/rules_go/go/tools/bazel_testing"
	it "github.com/tweag/rules_haskell/tests/integration_testing"
	"testing"
)

var testcase = `
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

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    components = {},
    local_snapshot = "@rules_haskell//:stackage_snapshot.yaml",
    packages = ["base"],
    stack_snapshot_json = "@rules_haskell//:stackage_snapshot.json" if not is_windows else None,
    tools = [],
    vendored_packages = {
        "ghc-paths": "@rules_haskell//tools/ghc-paths",
    },
)
-- BUILD.bazel --
"""Test compilation of a multiple interdependent Haskell modules with only core-package dependencies."""

load("@rules_haskell//haskell:defs.bzl", "haskell_library", "haskell_repl", "haskell_toolchain_library")
load("@rules_haskell//haskell/experimental:defs.bzl", "haskell_module")

haskell_toolchain_library(name = "base")

haskell_repl(
    name = "repl",
    deps = [":lib"],
)

haskell_module(
    name = "root",
    src = "Root.hs",
)

haskell_module(
    name = "branch_left",
    src = "BranchLeft.hs",
    deps = [
        ":root",
    ],
)

haskell_module(
    name = "branch_right",
    src = "BranchRight.hs",
    deps = [
        ":root",
    ],
)

haskell_module(
    name = "leaf",
    src = "Leaf.hs",
    deps = [
        ":branch_left",
        ":branch_right",
    ],
)

haskell_library(
    name = "lib",
    modules = [
        ":root",
        ":branch_left",
        ":branch_right",
        ":leaf",
    ],
    deps = [":base"],
)
-- BranchLeft.hs --
module BranchLeft where

import Root

branch_left :: Int
branch_left = 3 * root
-- BranchRight.hs --
module BranchRight where

import Root

branch_right :: Int
branch_right = 5 * root
-- Leaf.hs --
module Leaf where

import BranchLeft
import BranchRight

leaf :: Int
leaf = 7 * branch_left * branch_right
-- Root.hs --
module Root where

root :: Int
root = 2
`

func TestMain(m *testing.M) {
	it.TestMain(m, bt.Args{Main: testcase})
}

func TestHsModRepl(t *testing.T) {
	out, err := it.BazelOutput(it.Context.BazelBinary, "run", "//:repl", "--", "-ignore-dot-ghci", "-e", "leaf")
	if err != nil {
		t.Fatal(err)
	}
	it.AssertOutput(t, out, "420\n")
}
