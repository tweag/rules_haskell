package integration_testing

import (
        "bytes"
        "fmt"
        "os"
        "os/exec"
        "runtime"
        "testing"
        "github.com/bazelbuild/rules_go/go/tools/bazel_testing"
)

func TestMain(m *testing.M, workspace string) {
        bazel_testing.TestMain(m, bazel_testing.Args{
                Main: workspace + GenerateBazelrc(),
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
