package integration_testing

import (
        "bytes"
        "fmt"
        "os"
        "os/exec"
        "runtime"
        "strings"
        "testing"
        "github.com/bazelbuild/rules_go/go/tools/bazel"
        "github.com/bazelbuild/rules_go/go/tools/bazel_testing"
)

func TestMain(m *testing.M, workspace string) {
        if err := ParseArgs(); err != nil {
                fmt.Fprint(os.Stderr, err)
                return
        }
	defer exec.Command(Context.BazelBinary, "shutdown")

        bazel_testing.TestMain(m, bazel_testing.Args{
                Main: workspace + GenerateBazelrc(),
        })
}

func AssertOutput(t *testing.T, output []byte, expected string) {
        if string(output) != expected {
                t.Fatalf("output of bazel process is invalid.\nExpected: %v\n, Actual: %v\n", expected, string(output))
        }
}

var Context struct {
        Nixpkgs bool
        BazelBinary string
}

func ParseArgs() error {
        bazelPath := ""
        for _, arg := range os.Args {
                if strings.HasPrefix(arg, "nixpkgs=") {
                        fmt.Sscanf(arg, "nixpkgs=%t", &Context.Nixpkgs)
                } else if strings.HasPrefix(arg, "bazel_bin=") {
                        fmt.Sscanf(arg, "bazel_bin=%s", &bazelPath)
                }
        }
        bazelAbsPath, err := bazel.Runfile(bazelPath)
        Context.BazelBinary = bazelAbsPath
        return err
}

func GenerateBazelrc() string {
        bazelrc := "-- .bazelrc --\n"
        if Context.Nixpkgs {
                bazelrc += "build --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host\n"
        } else if runtime.GOOS == "windows" {
                bazelrc += "build --crosstool_top=@rules_haskell_ghc_windows_amd64//:cc_toolchain\n"
        }
        return bazelrc
}

func BazelEnv() []string {
        env := []string{}
	// It's important value of $HOME to be invariant between different integration test runs
        // and to be writable directory for bazel test. Probably TEST_TMPDIR is a valid choice
        // but documentation is not clear about it's default value
        // cmd.Env = append(cmd.Env, fmt.Sprintf("HOME=%s", os.Getenv("TEST_TMPDIR")))
        env = append(env, fmt.Sprintf("HOME=%s", os.TempDir()))
        if runtime.GOOS == "darwin" {
                env = append(env, "BAZEL_USE_CPP_ONLY_TOOLCHAIN=1")
                if Context.Nixpkgs {
                        env = append(env, "BAZEL_DO_NOT_DETECT_CPP_TOOLCHAIN=1")
                }
        }
        for _, e := range os.Environ() {
                // Filter environment variables set by the bazel test wrapper script.
                // These confuse recursive invocations of Bazel.
                if strings.HasPrefix(e, "TEST_") || strings.HasPrefix(e, "RUNFILES_") {
                        continue
                }
                env = append(env, e)
        }
        return env
}

func BazelCmd(bazelPath string, args ...string) *exec.Cmd {
        cmd := exec.Command(bazelPath)
        if bazel_testing.OutputUserRoot != "" {
                cmd.Args = append(cmd.Args, "--output_user_root="+bazel_testing.OutputUserRoot)
        }
        cmd.Args = append(cmd.Args, args...)
        cmd.Env = append(cmd.Env, BazelEnv()...)
        return cmd
}

func RunBazel(bazelPath string, args ...string) error {
        cmd := BazelCmd(bazelPath, args...)

        buf := &bytes.Buffer{}
        cmd.Stderr = buf
        err := cmd.Run()
        if eErr, ok := err.(*exec.ExitError); ok {
                eErr.Stderr = buf.Bytes()
                err = &bazel_testing.StderrExitError{Err: eErr}
        }
        return err
}

func BazelOutput(bazelPath string, args ...string) ([]byte, error) {
        cmd := BazelCmd(bazelPath, args...)
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
