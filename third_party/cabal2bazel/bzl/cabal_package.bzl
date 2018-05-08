# Copyright 2018 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Skylark build rules for cabal haskell packages.

To see all of the generated rules, run:
bazel query --output=build //third_party/haskell/{package}/v{version}:all
"""

load("@bazel_skylib//:lib.bzl", "collections")
load("//bzl:def.bzl", "haskell_library", "haskell_binary", "haskell_test")
load("//bzl:cabal_paths.bzl", "cabal_paths")
load("//bzl:hsc2hs.bzl", "hsc2hs")
load("//bzl:metadata.bzl", "package_key")
load("//bzl:providers.bzl", "forward_deps")
load("//bzl:hackage.bzl", "hackage")
load("//bzl:happy.bzl", "genhappy")
load("//bzl:alex.bzl", "genalex")

# A list of default packages which are provided by
# //third_party/haskell/ghc/... rather than //third_party/haskell/{package}.
# Note that this is redundant to
# //third_party/haskell/ghc:default-packages; however, the contents of that
# file aren't available during Skylark macros (or rule analysis).
_default_packages = [
    "Cabal",
    "array",
    "base",
    "binary",
    "bytestring",
    "containers",
    "deepseq",
    "directory",
    "filepath",
    "ghc",
    "ghc-boot",
    "ghc-boot-th",
    "ghc-prim",
    "ghci",
    "haskeline",
    "hoopl",
    "hpc",
    "integer-gmp",
    "pretty",
    "process",
    "template-haskell",
    "terminfo",
    "time",
    "transformers",
    "unix",
]

_conditions_default = "//conditions:default"

# A list of default packages which are hidden by default.
_hidden_default_packages = ["ghc"]

# Extra GHC arguments to undo the google3 defaults which might otherwise break
# some Cabal packages.
_undo_default_options = [
    "-XForeignFunctionInterface",
    "-XNoScopedTypeVariables",
]
_ignored_extensions = [
    # Deprecated in July 2014: https://ghc.haskell.org/trac/ghc/ticket/9242#comment:28
    "OverlappingInstances",
]

# cabal_haskell_library is a build rule which is a thin wrapper around
# haskell_library, tagging it with a version number.
# Usage:
#   haskell_library(
#       name = "foo-lib",
#       ...,
#   )
#   cabal_haskell_library(
#       name = "foo",
#       version = "1.2.3",
#   )
#   # Forward a library, in particular used for version-agnostic libraries.
#   # Automaticaly picks up the version number from the "library" attribute.
#   cabal_haskell_library(
#      library = ":foo-forwarded",
#   )
def _impl_cabal_haskell_library(ctx):
  if ctx.attr.version:
    cabal_id = struct(
        name=ctx.label.name,
        version=ctx.attr.version)
  else:
    cabal_id = ctx.attr.library.haskell_cabal_id

  return forward_deps(
      ctx,
      ctx.attr.deps,
      {"haskell_cabal_id": cabal_id})

_cabal_haskell_library = rule(
    implementation=_impl_cabal_haskell_library,
    attrs={
        "deps": attr.label_list(),
        "library": attr.label(),
        "version": attr.string(),
    },
)

def cabal_haskell_library(name, library, version=""):
  # We want cabal_haskell_library to accept only one dependency, the library
  # that it is forwarding, so we use a single label attr called "library"
  # instead of "deps". However, bazel treats "deps" specially when finding
  # transitive data dependencies and if we don't include the "library"
  # dependency in a "deps" attribute, we must manually forward all data
  # dependencies. To avoid that, we include a "deps" attr and use this macro to
  # enforce that "deps == [library]".
  _cabal_haskell_library(
      name=name,
      deps=[library],
      library=library,
      version=version,
  )

# Cabal macro generation target name ends with this.
_macros_suffix = "-macros"

# The _cabal_haskell_macros rule generates a file containing Cabal
# MIN_VERSION_* macros of all of the specified dependencies, as well as some
# other Cabal macros.
# For more details, see //bzl/cabal/GenerateCabalMacros.hs.
# Args:
#   deps: A list of cabal_haskell_library rules.
#   default_packages: A list of names of default packages that
#     this package depends on; e.g., "base".
def _impl_cabal_haskell_macros(ctx):
  if not ctx.label.name.endswith(_macros_suffix):
    fail("Macros target ends with unexpected suffix.")
  ctx.action(
      outputs=[ctx.outputs.out],
      inputs=[ctx.file._default_packages_file, ctx.executable._gen_cabal_macros],
      progress_message="Generating Haskell Cabal macros for %s" % str(ctx.label),
      mnemonic="HaskellGenerateCabalMacros",
      command=(
          " ".join(
              [ctx.executable._gen_cabal_macros.path,
               # Ideally the package key would be passed in as an attribute, but
               # caller of the 'cabal_haskell_macros' rule (ultimately
               # 'cabal_haskell_package') is not (yet) a proper rule, so can't
               # access the ctx (and so the Bazel package) from there.
               #
               # Therefore resorting to the hack of removing the "-macros"
               # suffix added by the callsite of this rule.
               package_key(ctx, ctx.label.name[:-len(_macros_suffix)]),
               ctx.file._default_packages_file.path,
              ] + ctx.attr.default_packages
              + [p.haskell_cabal_id.name + "-"
                 + p.haskell_cabal_id.version
                 for p in ctx.attr.deps])
          + " > " + ctx.outputs.out.path),
  )

_cabal_haskell_macros = rule(
    implementation=_impl_cabal_haskell_macros,
    attrs={
        "deps": attr.label_list(providers=["haskell_cabal_id"]),
        "default_packages": attr.string_list(),
        "_default_packages_file": attr.label(
            single_file=True,
            default=Label("//third_party/haskell/ghc:default_packages"),
        ),
        "_gen_cabal_macros": attr.label(
            executable=True,
            cfg="host",
            default=Label("//bzl/cabal:GenerateCabalMacros"),
        ),
    },
    outputs={"out": "%{name}.h"},
)

def _paths_module(desc):
  return "Paths_" + desc.package.pkgName.replace("-","_")

def _glob_modules(src_dir, extension):
  """List Haskell files under the given directory with this extension.

  Args:
    src_dir: A subdirectory relative to this package.
    extension: A file extension; for example, ".hs" or ".hsc".
  Returns:
    A list of 3-tuples containing:
      1. The original file, e.g., "srcs/Foo/Bar.hsc"
      2. The Haskell module name, e.g., "Foo.Bar"
      3. The preprocessed Haskell file name, e.g., "Foo/Bar.hs"
  """
  outputs = []
  for f in native.glob([src_dir + "**/*" + extension]):
    m = f[len(src_dir):-len(extension)]
    outputs += [(f, m.replace("/", "."), m + ".hs")]
  return outputs

def _conditions_dict(d):
  return d.select if hasattr(d, "select") else {_conditions_default: d}

def _get_build_attrs(name, build_info, desc, generated_srcs_dir, extra_modules, cc_deps=[], version_overrides=None, ghcopts=[]):
  """Get the attributes for a particular library or binary rule.

  Args:
    name: The name of this component.
    build_info: A struct of the Cabal BuildInfo for this component.
    desc: A struct of the Cabal PackageDescription for this package.
    generated_srcs_dir: Location of autogenerated files for this rule,
      e.g., "dist/build" for libraries.
    extra_modules: exposed-modules: or other-modules: in the package description
    cc_deps: External cc_libraries that this rule should depend on.
    version_overrides: Override the default version of specific dependencies;
      see cabal_haskell_package for more details.
    ghcopts: Extra GHC options.
  Returns:
    A dictionary of attributes (e.g. "srcs", "deps") that can be passed
    into a haskell_library or haskell_binary rule.
  """

  # Preprocess and collect all the source files by their extension.
  # module_map will contain a dictionary from module names ("Foo.Bar")
  # to the preprocessed source file ("src/Foo/Bar.hs").
  module_map = {}

  for d in build_info.hsSourceDirs + [generated_srcs_dir]:
    base = _prefix_dir_base(d)
    for f,m,_ in _glob_modules(base, ".hsc"):
      module_map[m] = f[:-4] + ".hs"
      hsc2hs(
          name = "hsc2hs_" + f,
          src = f,
          deps = [":" + name + "-cbits"],
      )
    for f,m,out in _glob_modules(base, ".x"):
      module_map[m] = out
      genalex(
          src = f,
          out = out,
      )
    for f,m,out in _glob_modules(base, ".y") + _glob_modules(base, ".ly"):
      module_map[m] = out
      genhappy(
          src = f,
          out = out,
      )
    # Raw source files.  Include them last, to override duplicates (e.g. if a
    # package contains both a Happy Foo.y file and the corresponding generated
    # Foo.hs).
    for f,m,_ in _glob_modules(base, ".hs") + _glob_modules(base, ".lhs"):
      module_map[m] = f

  # Collect the source files for each module in this Cabal component.
  # srcs is a mapping from "select()" conditions (e.g. //third_party/haskell/ghc:ghc-8.0.2) to a list of source files.
  # Turn "boot_srcs" and others to dicts if there is a use case.
  srcs = {}
  # Keep track of .hs-boot files specially.  GHC doesn't want us to pass
  # them as command-line arguments; instead, it looks for them next to the
  # corresponding .hs files.
  boot_srcs = []
  deps = {}
  paths_module = _paths_module(desc)
  extra_modules_dict = _conditions_dict(extra_modules)
  other_modules_dict = _conditions_dict(build_info.otherModules)
  for condition in depset(extra_modules_dict.keys() + other_modules_dict.keys()):
    srcs[condition] = []
    deps[condition] = []
    for m in (extra_modules_dict.get(condition, []) +
              other_modules_dict.get(condition, [])):
      if m == paths_module:
        deps[condition] += [":" + paths_module]
      elif m in module_map:
        srcs[condition] += [module_map[m]]
        # Get ".hs-boot" and ".lhs-boot" files.
        boot_srcs += native.glob([module_map[m] + "-boot"])
      else:
        fail("Missing module %s for %s" % (m, name))

  # Collect the options to pass to ghc.
  extra_ghcopts = ghcopts
  ghcopts = _undo_default_options
  all_extensions = [ ext for ext in
                     ([build_info.defaultLanguage]
                      if build_info.defaultLanguage else ["Haskell98"])
                     + build_info.defaultExtensions
                     + build_info.oldExtensions ]
  for ext in _ignored_extensions:
    if ext in all_extensions:
      print("GHC Extension %s has been ignored when building %s." % (ext, name))
  ghcopts = ghcopts + ["-X" + ext for ext in all_extensions
                       if ext not in _ignored_extensions ]

  ghcopt_blacklist = ["-Wall","-Wwarn","-w","-Werror"]
  for (compiler,opts) in build_info.options:
    if compiler == "ghc":
      ghcopts += [o for o in opts if o not in ghcopt_blacklist]
  ghcopts += ["-w", "-Wwarn"]  # -w doesn't kill all warnings...

  # Collect the dependencies.
  default_packages = []
  explicit_deps_idx = len(deps[_conditions_default])
  for condition, ps in _conditions_dict(build_info.targetBuildDepends).items():
    if condition not in deps:
      deps[condition] = []
    for p in ps:
      if p.name in _default_packages:
        if p.name in _hidden_default_packages:
          ghcopts += ["-package", p.name]
        if p.name not in default_packages:
          default_packages.append(p.name)
      elif p.name == desc.package.pkgName:
        # Allow executables to depend on the library in the same package.
        deps[condition] += [":" + p.name]
      elif version_overrides and p.name in version_overrides:
        deps[condition] += [
            "//third_party/haskell/" + p.name.replace("-","_")
            + "/v" + version_overrides[p.name].replace(".", "_")
            + ":" + p.name]
      else:
        deps[condition] += [hackage(p.name)]

  # Generate the macros for these dependencies.
  # TODO: Maybe remove the MIN_VERSION_<package> macro generation,
  #   since GHC 8 itself (not Cabal) generates these. But not the
  #   CURRENT_PACKAGE_KEY macro?
  #   See https://ghc.haskell.org/trac/ghc/ticket/10970.
  _cabal_haskell_macros(
      name = name + _macros_suffix,
      deps = deps[_conditions_default][explicit_deps_idx:],
      default_packages = default_packages,
  )
  ghcopts += ["-optP-include", "-optP$(location :%s)" % (name + _macros_suffix)]

  ghcopts += ["-optP" + o for o in build_info.cppOptions]

  # Generate a cc_library for this package.
  # TODO(judahjacobson): don't create the rule if it's not needed.
  # TODO(judahjacobson): Figure out the corner case logic for some packages.
  # In particular: JuicyPixels, cmark, ieee754.
  install_includes = native.glob(
      [_prefix_dir(d, f) for d in build_info.includeDirs
       for f in build_info.installIncludes])
  headers = depset(
      [f for f in native.glob(desc.extraSrcFiles) if f.endswith(".h")] + install_includes)
  ghcopts += ["-I" + native.package_name() + "/" + d for d in build_info.includeDirs]
  lib_name = name + "-cbits"
  for xs in deps.values():
    xs.append(":" + lib_name)
  native.cc_library(
      name = lib_name,
      srcs = build_info.cSources,
      includes = build_info.includeDirs,
      copts = ([o for o in build_info.ccOptions if not o.startswith("-D")]
               + ["-w"]),
      defines = [o[2:] for o in build_info.ccOptions if o.startswith("-D")],
      textual_hdrs = list(headers),
      deps = ["//third_party/haskell/ghc:headers"] + cc_deps,
  )

  if boot_srcs:
    ghcopts += ["-i" + native.package_name() + "/" + d for d in build_info.hsSourceDirs]

  return {
      "srcs": srcs,
      "deps": deps,
      "ghcopts": ghcopts + extra_ghcopts,
      "extra_src_files": [":" + name + _macros_suffix] + collections.uniq(
          boot_srcs + install_includes + native.glob(desc.extraSrcFiles)),
  }

def _prefix_dir_base(d):
  if d and d != ".":
    if d.endswith("/"):
      return d  # Skylark doesn't like consecutive slashes
    else:
      return d + "/"
  else:
    return ""

def _prefix_dir(dir_path, file_path):
  """Prefix the file with the given directory, without extra "/"s."""
  return _prefix_dir_base(dir_path) + file_path

def cabal_haskell_package(description=None, cc_deps=[], data=[], version_overrides={}, name=None, cbits=True, ghcopts=[], **kwargs):
  """Create rules for building a Cabal package.

  Args:
    description: A Skylark struct generated by cabal2build representing a
      .cabal file's contents.
    cc_deps: Additional google3 cc_library targets that this package should
      depend on.
    data: "data" for haskell_library.
    version_overrides: Specify that this rule should use a non-default version
      of some dependencies.  (NOTE: This should be avoided when possible; see
      go/thirdpartyhaskell for details.)  A dictionary mapping string package
      names to versions, e.g., {"haskell-src-exts": "1.16.0"}.
    cbits: Whether to insert object files from lib<package>-cbits.a into the produced .a .
    ghcopts: Extra GHC options.
  """
  name = description.package.pkgName

  # TODO(judahjacobson): don't generate a Paths_ library unless it's needed.
  cabal_paths(
      name = _paths_module(description),
      package = name.replace("-","_"),
      version = [int(v) for v in description.package.pkgVersion.split(".")],
      data_dir = description.dataDir,
      data = native.glob([_prefix_dir(description.dataDir, d) for d in description.dataFiles]),
  )

  lib = description.library
  if lib and lib.libBuildInfo.buildable:
    lib_attrs = _get_build_attrs(name + "-lib", lib.libBuildInfo, description,
                                 "dist/build",
                                 lib.exposedModules,
                                 cc_deps,
                                 version_overrides=version_overrides,
                                 ghcopts=ghcopts)
    lib_name = name + "-lib"
    srcs = lib_attrs.pop("srcs")
    deps = lib_attrs.pop("deps")
    haskell_library(
        name = lib_name,
        data = data,
        srcs = select(srcs),
        deps = select(deps),
        extra_objs = [":" + name + "-lib-cbits"] if cbits else [],
        **lib_attrs
    )

    cabal_haskell_library(
        name=name,
        library=":" + lib_name,
        version = description.package.pkgVersion,
    )

  for exe in description.executables:
    if not exe.buildInfo.buildable:
      continue
    exe_name = exe.exeName
    # Avoid a name clash with the library.  For stability, make this logic
    # independent of whether the package actually contains a library.
    if exe_name == name:
      exe_name = name + "_bin"
    paths = _paths_module(description)
    attrs = _get_build_attrs(exe_name, exe.buildInfo, description,
                             "dist/build/%s/%s-tmp" % (name, name),
                             # Some packages (e.g. happy) don't specify the
                             # Paths_ module explicitly.
                             [paths] if paths not in exe.buildInfo.otherModules
                             else [],
                             version_overrides=version_overrides,
                             ghcopts=ghcopts)
    srcs = attrs.pop("srcs")
    deps = attrs.pop("deps")

    [full_module_path] = native.glob(
        [_prefix_dir(d, exe.modulePath) for d in exe.buildInfo.hsSourceDirs])
    for xs in srcs.values():
      if full_module_path not in xs:
        xs.append(full_module_path)

    haskell_binary(
        name = exe_name,
        srcs = select(srcs),
        deps = select(deps),
        **attrs
    )
