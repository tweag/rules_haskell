// Package test_filegroup generates an "all_files" filegroup target
// in each package. This target globs files in the same package and
// depends on subpackages.
//
// These rules are used for testing with haskell_bazel_test.
//
// This extension is experimental and subject to change. It is not included
// in the default Gazelle binary.
package test_filegroup

import (
	"log"
	"io/ioutil"
	"path"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/repo"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

const testFilegroupName = "test_filegroup"

type testFilegroupLang struct{}

func NewLanguage() language.Language {
	return &testFilegroupLang{}
}

func (*testFilegroupLang) Name() string { return testFilegroupName }

func (*testFilegroupLang) Kinds() map[string]rule.KindInfo {
	return kinds
}

func (*testFilegroupLang) Loads() []rule.LoadInfo { return nil }

func (*testFilegroupLang) Fix(c *config.Config, f *rule.File) {}

func (*testFilegroupLang) Imports(c *config.Config, r *rule.Rule, f *rule.File) []resolve.ImportSpec {
	return nil
}

func (*testFilegroupLang) Embeds(r *rule.Rule, from label.Label) []label.Label { return nil }

func (*testFilegroupLang) Resolve(c *config.Config, ix *resolve.RuleIndex, rc *repo.RemoteCache, r *rule.Rule, imports interface{}, from label.Label) {
}

var kinds = map[string]rule.KindInfo{
	"filegroup": {
		NonEmptyAttrs:  map[string]bool{"srcs": true, "deps": true},
		MergeableAttrs: map[string]bool{"srcs": true},
	},
}

// GetFilegroupConfig returns the filegroup configuration.  If the
// filegroup extension was not run, it will return nil.
func GetFilegroupConfig(c *config.Config) *FilegroupConfig {
	fg := c.Exts[testFilegroupName]
	if fg == nil {
		return nil
	}
	return fg.(*FilegroupConfig)
}


func IsBuildFile(fileName string, buildFileNames []string) bool {
	for _,n := range buildFileNames {
		if n == fileName {
			return true
		}
	}
	return false
}

/*
These are the files taken from buildifier_test PR
https://github.com/bazelbuild/buildtools/pull/929
   "*.BUILD",
	"*.bzl",
	"*.sky",
	"BUILD",
	"BUILD.*.bazel",
	"BUILD.*.oss",
	"BUILD.bazel",
	"WORKSPACE",
	"WORKSPACE.*.bazel",
	"WORKSPACE.*.oss",
	"WORKSPACE.bazel",
And these are from buildifier itself
https://github.com/bazelbuild/buildtools/blob/master/buildifier/internal/factory.bzl#L80
                    ".bazel",
                    ".bzl",
                    ".oss",
                    ".sky",
                    "BUILD",
                    "WORKSPACE",
and then they get classified
https://github.com/bazelbuild/buildtools/blob/f2aed9ee205d62d45c55cfabbfd26342f8526862/build/lex.go#L109
*/
func (*testFilegroupLang) GenerateRules(args language.GenerateArgs) language.GenerateResult {
	// we don't want to create new packages
	// if there's no BUILD file, do nothing
	if args.File == nil {
		return language.GenerateResult{
			Gen: []*rule.Rule{},
			Imports: []interface{}{},
		}
	}

	c := args.Config
	fgConf := GetFilegroupConfig(args.Config)
	filegroupName := fgConf.Name

	r := rule.NewRule("filegroup", filegroupName)
	// note: the slice might end up larger than allocated here.  append anyways
	srcs := make([]string, 0, len(args.Subdirs)+len(args.RegularFiles))
	var getSubSrcs func(string) []string
	getSubSrcs = func(name string) []string {
		files, err := ioutil.ReadDir(path.Join(args.Dir,name))
		if err != nil {
			log.Fatal(err)
		}
		// if the folder begins a new workspace, we don't touch it
		// to avoid cross workspace boundaries
		var IsWorkspace func(string) bool
		IsWorkspace = func(f string) bool {
			return f == "WORKSPACE" || f == "WORKSPACE.bazel"
		}
		for _,file := range files {
			if IsWorkspace(file.Name()) {
				return nil
			}
		}
		var dirIsPackage func(string) bool
		dirIsPackage = func(f string) bool {
			return IsBuildFile(f,c.ValidBuildFileNames)
		}
		srcs := make([]string,0,len(files))
		for _,file := range files {
			fileName := file.Name()
			if file.IsDir() {
				srcs = append(srcs,getSubSrcs(path.Join(name, fileName))...)
			} else if IsWorkspace(fileName) {
				return nil
			} else if dirIsPackage(fileName) {
				pkg := path.Join(args.Rel, name)
				return []string{"//"+pkg+":"+filegroupName}
			} else {
				shouldAppend := false
				switch fgConf.Files {
				case All:
					shouldAppend = true
				case Starlark:
					fileExt := path.Ext(fileName)
					shouldAppend =
						fileExt == ".bazel" || fileExt == ".bzl" || fileExt == ".oss" || fileExt == ".sky" || fileName == "WORKSPACE" || IsBuildFile(fileName,c.ValidBuildFileNames)
				default:
					log.Fatal("missing or invalid files option: %d", fgConf.Files)
				}
				if shouldAppend {
					srcs = append(srcs, path.Join(name, fileName))
				}
			}
		}
		return srcs
	}
	for _, f := range args.RegularFiles {
		shouldAppend := false
		switch fgConf.Files {
		case All:
			shouldAppend = true
		case Starlark:
			fileExt := path.Ext(f)
			shouldAppend =
				fileExt == ".bazel" || fileExt == ".bzl" || fileExt == ".oss" || fileExt == ".sky" || f == "WORKSPACE" || IsBuildFile(f,c.ValidBuildFileNames)
		default:
			log.Fatal("missing or invalid files option: %d", fgConf.Files)
		}
		if shouldAppend {
			srcs = append(srcs, f)
		}
	}
	for _, f := range args.Subdirs {
		srcs = append(srcs,getSubSrcs(f)...)
	}
	r.SetAttr("srcs", srcs)
	r.SetAttr("testonly", true)
	if !args.File.HasDefaultVisibility() {
		r.SetAttr("visibility", []string{"//visibility:public"})
	}
	return language.GenerateResult{
		Gen:     []*rule.Rule{r},
		Imports: []interface{}{nil},
	}
}
