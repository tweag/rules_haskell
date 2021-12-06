package hs_bin_repl_test

import (
        "testing"
        "github.com/bazelbuild/rules_go/go/tools/bazel_testing"
)

func TestMain(m *testing.M) {
        bazel_testing.TestMain(m, bazel_testing.Args{
                Main: `
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
    attribute_path = "haskell.compiler.ghc8104",
    repository = "@rules_haskell//nixpkgs:default.nix",
    version = "8.10.4",
)

load("@rules_haskell//haskell:toolchain.bzl", "rules_haskell_toolchains")

rules_haskell_toolchains(version = "8.10.4")

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

-- .bazelrc --
build --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host
-- Quux.hs --
module Main (main) where

import QuuxLib (message)

main :: IO ()
main = putStrLn message
-- QuuxLib.hs --
module QuuxLib (message) where

message :: String
message = "Hello GHCi!"
`,
        })
}

func AssertOutput(t *testing.T, output []byte, expected string) {
        if string(output) != expected {
                t.Fatalf("output of bazel process is invalid.\nExpected: %v\n, Actual: %v\n", expected, string(output))
        }
}

func TestHsBinRepl(t *testing.T) {
        out, err := bazel_testing.BazelOutput("run", "//:hs-bin@repl", "--", "-ignore-dot-ghci", "-e", ":main")
        if err != nil {
                t.Fatal(err)
        }
        AssertOutput(t, out, "Hello GHCi!\n")
}
