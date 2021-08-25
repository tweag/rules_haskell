// Package test_filegroup generates an "all_files" filegroup target
// in each package. This target globs files in the same package and
// depends on subpackages.
//
// These rules are used for testing with go_bazel_test.
//
// This extension is experimental and subject to change. It is not included
// in the default Gazelle binary.
package test_filegroup

import (
	"flag"
	"fmt"
	"log"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

type FilegroupConfig struct {
	// the name given to the file group, e.g. all_files, starlark_files
	Name string

	// the files to include in the filegroup
	Files Files
}

// Files tells which files should be included in the filegroup
type Files int

const (
	// includes all files in the filegroup
	All Files = iota
	// includes files from args.Config.ValidBuildFileNames, *.bazel,
	// *.bzl, *.oss, WORKSPACE
	Starlark
)

func FilesFromString(s string) (Files, error) {
	switch s {
	case "all":
		return All, nil
	case "starlark":
		return Starlark, nil
	default:
		return 0, fmt.Errorf("unrecognized files option: %q", s)
	}
}

func (f Files) String() string {
	switch f {
	case All:
		return "all"
	case Starlark:
		return "starlark"
	default:
		log.Panicf("unknown files option: %d", f)
		return ""
	}
}

type filesFlag struct {
	files *Files
}
func (f *filesFlag) Set(value string) error {
	if files, err := FilesFromString(value); err != nil {
		return err
	} else {
		*f.files = files
		return nil
	}
}

func (f *filesFlag) String() string {
	var files Files
	if f != nil && f.files != nil {
		files = *f.files
	}
	return files.String()
}

func (*testFilegroupLang) RegisterFlags(fs *flag.FlagSet, cmd string, c *config.Config) {
	fg := &FilegroupConfig{}
	c.Exts[testFilegroupName] = fg

	fs.StringVar(&fg.Name, "name", "files", "name for the generated filegroup")
	fs.Var(&filesFlag{&fg.Files}, "files", "all: includes all files in a directory.\n\tstarlark: includes starlark files, *.bazel, *.bzl, *.oss, WORKSPACE and the build files listed in build_file_name")
}

func (*testFilegroupLang) CheckFlags(fs *flag.FlagSet, c *config.Config) error { return nil }

func (*testFilegroupLang) KnownDirectives() []string { return nil }

func (*testFilegroupLang) Configure(c *config.Config, rel string, f *rule.File) {}

