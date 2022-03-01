package hs_override_stack_test

import (
	bt "github.com/bazelbuild/rules_go/go/tools/bazel_testing"
	it "github.com/tweag/rules_haskell/tests/integration_testing"
	"os"
	"path"
	"strings"
	"testing"
)

var testcase = `
-- WORKSPACE --
local_repository(
    name = "rules_haskell",
    path = "../rules_haskell",
)

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

load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot",
    "use_stack",
)

# use dummy stack that only passes version check.
# this should override the default behavior of 'stack_snapshot'
# to use whatever is available in the environment.
use_stack("//:stack")

stack_snapshot(
    name = "stackage",
    local_snapshot = "@rules_haskell//:stackage_snapshot.yaml",
    packages = [
        "base",
    ],
)

-- stack --
#!/bin/sh
echo 2.3.1

-- BUILD.bazel --
load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_test",
)

haskell_test(
    name = "hs-bin",
    srcs = ["Quux.hs"],
    visibility = ["//visibility:public"],
    deps = [ "@stackage//:base" ],
)

-- Quux.hs --
module Main (main) where

main :: IO ()
main = putStrLn "Hello GHCi!"
`

func TestMain(m *testing.M) {
	it.TestMain(m, bt.Args{
		Main: testcase,
		SetUp: func() (err error) {
			// set executable bit on dummy script, since `bazel_testing` does not allow it
			// https://github.com/bazelbuild/rules_go/issues/2281
			dir, err := os.Getwd()
			if err != nil {
				return
			}
			stack := path.Join(dir, "stack")
			info, err := os.Stat(stack)
			if err != nil {
				return
			}
			return os.Chmod(stack, 0700|info.Mode())
		},
	})
}

func TestHsBinRepl(t *testing.T) {
	_, err := it.BazelOutput(it.Context.BazelBinary, "run", "//:hs-bin@repl", "--", "-ignore-dot-ghci", "-e", ":main")
	out := string(err.(*bt.StderrExitError).Err.Stderr)
	if err == nil {
		t.Fatal(out, "build succeeds, but should fail due invalid `stack` binary")
	}
	if !strings.Contains(out, "parsing JSON failed") {
		t.Fatal(out, "build does not use specified dummy `stack`")
	}
}
