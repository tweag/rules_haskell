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

func TestMain(m *testing.M, args bazel_testing.Args) {
	if err := ParseArgs(); err != nil {
		fmt.Fprint(os.Stderr, err)
		return
	}
	defer exec.Command(Context.BazelBinary, "shutdown")

	args.Main += GenerateBazelrc()
	bazel_testing.TestMain(m, args)
}

func AssertOutput(t *testing.T, output []byte, expected string) {
	if string(output) != expected {
		t.Fatalf("output of bazel process is invalid.\n%-10s%v\n%-10s%v\n", "Expected:", expected, "Actual:", string(output))
	}
}

var Context struct {
	Nixpkgs     bool
	BazelBinary string
}

func ParseArgs() error {
	bazelPath := ""
	fmt.Print("OSARGS are ", os.Args)
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
	bazelrc := `
-- .bazelrc --
build:linux-nixpkgs --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host
build:linux-nixpkgs --incompatible_enable_cc_toolchain_resolution
build:macos-nixpkgs --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host
build:macos-nixpkgs --incompatible_enable_cc_toolchain_resolution
build:linux-bindist --incompatible_enable_cc_toolchain_resolution
build:macos-bindist --incompatible_enable_cc_toolchain_resolution
build:windows-bindist --crosstool_top=@rules_haskell_ghc_windows_amd64//:cc_toolchain
`
	if bazel_testing.OutputUserRoot != "" {
		bazelrc += fmt.Sprintf("startup: --output_user_root=%s\n", bazel_testing.OutputUserRoot)
	}
	return bazelrc
}

func BazelEnv() []string {
	env := []string{}
	// It's important that the value of $HOME is invariant between different integration test runs
	// and that the directory is writable for bazel test.
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

func insertBazelFlags(args []string, flags ...string) []string {
	for i, arg := range args {
		switch arg {
		case "build", "test", "run":
			return append(append(append([]string{}, args[:i+1]...), flags...), args[i+1:]...)
		default:
			continue
		}
	}
	return args
}

func BazelConfig() string {
	switch os := runtime.GOOS; os {
	case "linux":
		if Context.Nixpkgs {
			return "linux-nixpkgs"
		} else {
			return "linux-bindist"
		}
	case "darwin":
		if Context.Nixpkgs {
			return "macos-nixpkgs"
		} else {
			return "macos-bindist"
		}
	case "windows":
		return "windows-bindist"
	default:
		panic(fmt.Sprintf("Unknown OS name: %s", os))
	}
}

func BazelCmd(bazelPath string, args ...string) *exec.Cmd {
	cmd := exec.Command(bazelPath)
	args = insertBazelFlags(args, "--config", BazelConfig())
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
