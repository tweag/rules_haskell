package hs_bin_repl_test

import (
        "bytes"
        "fmt"
        "os"
        "os/exec"
        "runtime"
        "testing"
        "github.com/bazelbuild/rules_go/go/tools/bazel_testing"
)



func Workspace() string {
        return `
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
}

func TestMain(m *testing.M) {
        bazel_testing.TestMain(m, bazel_testing.Args{
                Main: Workspace() + GenerateBazelrc(),
        })
}

func AssertOutput(t *testing.T, output []byte, expected string) {
        if string(output) != expected {
                t.Fatalf("output of bazel process is invalid.\nExpected: %v\n, Actual: %v\n", expected, string(output))
        }
}

func UseNixpkgs() bool {
        for _, arg := range os.Args {
                if arg == "nixpkgs=true" {
                        return true
                }
        }
        return false
}

func GenerateBazelrc() string {
        bazelrc := "-- .bazelrc --\n"
        if UseNixpkgs() {
                bazelrc += "build --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host\n"
        } else if runtime.GOOS == "windows" {
                bazelrc += "build --crosstool_top=@rules_haskell_ghc_windows_amd64//:cc_toolchain\n"
        }
        return bazelrc
}

func BazelOutput(args ...string) ([]byte, error) {
        cmd := bazel_testing.BazelCmd(args...)

        // It's important value of $HOME to be invariant between different integration test runs
        // and to be writable directory for bazel test. Probably TEST_TMPDIR is a valid choice
        // but documentation is not clear about it's default value
        // cmd.Env = append(cmd.Env, fmt.Sprintf("HOME=%s", os.Getenv("TEST_TMPDIR")))
        cmd.Env = append(cmd.Env, fmt.Sprintf("HOME=%s", os.TempDir()))
        if runtime.GOOS == "darwin" {
                cmd.Env = append(cmd.Env, "BAZEL_USE_CPP_ONLY_TOOLCHAIN=1")
        }
        stdout := &bytes.Buffer{}
        stderr := &bytes.Buffer{}
        cmd.Stdout = stdout
        cmd.Stderr = stderr
        err := cmd.Run()
        if eErr, ok := err.(*exec.ExitError); ok {
                eErr.Stderr = stderr.Bytes()
                err = &bazel_testing.StderrExitError{Err: eErr}
        }
        return stdout.Bytes(), err
}

func TestHsBinRepl(t *testing.T) {
        out, err := BazelOutput("run", "//:hs-bin@repl", "--", "-ignore-dot-ghci", "-e", ":main")
        if err != nil {
                t.Fatal(err)
        }
        AssertOutput(t, out, "Hello GHCi!\n")
}
