"""Core Haskell rules"""

load(":providers.bzl",
  "HaskellBuildInfo",
  "HaskellLibraryInfo",
  "HaskellBinaryInfo",
  "CcSkylarkApiProviderHacked",
)

load(":actions.bzl",
  "compile_haskell_bin",
  "link_haskell_bin",
  "compile_haskell_lib",
  "link_static_lib",
  "link_dynamic_lib",
  "create_ghc_package",
  "gather_dep_info",
  "get_pkg_id",
)

load(":set.bzl", "set")
load("@bazel_skylib//:lib.bzl", "paths")

load(":path_utils.bzl",
     "import_hierarchy_root",
)

# For re-exports:
load(":haddock.bzl",
  _haskell_doc = "haskell_doc",
)
load(":lint.bzl",
  _haskell_lint = "haskell_lint",
)
load(":toolchain.bzl",
  _haskell_toolchain = "haskell_toolchain",
)
load (":ghc_bindist.bzl",
  _ghc_bindist = "ghc_bindist",
)
load(":ghci-repl.bzl",
  _haskell_repl = "haskell_repl",
)
load(":cc.bzl",
  _haskell_cc_import = "haskell_cc_import",
  _cc_haskell_import = "cc_haskell_import",
)

_haskell_common_attrs = {
  "src_strip_prefix": attr.string(
    doc = "Directory in which module hierarchy starts.",
  ),
  "srcs": attr.label_list(
    allow_files = FileType([".hs", ".hsc", ".lhs", ".hs-boot", ".lhs-boot", ".h"]),
    doc = "Haskell source files.",
  ),
  "deps": attr.label_list(
    doc = "List of other Haskell libraries to be linked to this target.",
  ),
  "data": attr.label_list(
    doc = "See [Bazel documentation](https://docs.bazel.build/versions/master/be/common-definitions.html#common.data).",
    cfg = "data",
  ),
  "compiler_flags": attr.string_list(
    doc = "Flags to pass to Haskell compiler.",
  ),
  "prebuilt_dependencies": attr.string_list(
    doc = "Non-Bazel supplied Cabal dependencies.",
  ),
  # XXX Consider making this private. Blocked on
  # https://github.com/bazelbuild/bazel/issues/4366.
  "version": attr.string(
    default = "1.0.0",
    doc = "Library/binary version. Internal - do not use."
  ),
  "_ghc_defs_cleanup": attr.label(
    allow_single_file = True,
    default = Label("@io_tweag_rules_haskell//haskell:ghc-defs-cleanup.sh"),
  )
}

def _haskell_binary_impl(ctx):
  c = compile_haskell_bin(ctx)

  binary, so_symlink_prefix = link_haskell_bin(ctx, c.object_dyn_files)
  dep_info = gather_dep_info(ctx)

  so_symlinks = {}

  for lib in set.to_list(dep_info.external_libraries):
    so_symlinks[paths.join(so_symlink_prefix, paths.basename(lib.path))] = lib

  return [
    dep_info, # HaskellBuildInfo
    HaskellBinaryInfo(
      source_files = c.source_files,
      modules = c.modules,
      binary = binary,
    ),
    DefaultInfo(
      executable = binary,
      files = depset([binary]),
      runfiles = ctx.runfiles(symlinks=so_symlinks, collect_data = True),
    ),
  ]

def _mk_binary_rule(**kwargs):
  """Generate a rule that compiles a binary.

  This is useful to create variations of a Haskell binary compilation
  rule without having to copy and paste the actual `rule` invocation.

  Args:
    **kwargs: Any additional keyword arguments to pass to `rule`.

  Returns:
    Rule: Haskell binary compilation rule.
  """
  return rule(
    _haskell_binary_impl,
    executable = True,
    attrs = dict(
      _haskell_common_attrs,
      generate_so = attr.bool(
        default = False,
        doc = "Whether to generate also a .so version of executable.",
      ),
      main_function = attr.string(
        default = "Main.main",
        doc = "Location of `main` function.",
      ),
      main_file = attr.label(
        allow_single_file = FileType([".hs", ".hsc", ".lhs"]),
        doc = "File containing `Main` module.",
      )
    ),
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
    **kwargs
  )

haskell_test = _mk_binary_rule(test = True)
"""Build a test suite.

Additionally, it accepts [all common bazel test rule
fields][bazel-test-attrs]. This allows you to influence things like
timeout and resource allocation for the test.

[bazel-test-attrs]: https://docs.bazel.build/versions/master/be/common-definitions.html#common-attributes-tests
"""

haskell_binary = _mk_binary_rule()
"""Build an executable from Haskell source.

Example:
  ```bzl
  haskell_binary(
      name = "main",
      srcs = ["Main.hs", "Other.hs"],
      deps = ["//lib:some_lib"]
  )
  ```
"""

def _haskell_library_impl(ctx):
  c = compile_haskell_lib(ctx)

  static_library = link_static_lib(ctx, c.object_files)
  dynamic_library = link_dynamic_lib(ctx, c.object_dyn_files)

  dep_info = gather_dep_info(ctx)

  exposed_modules = set.empty()
  other_modules = set.from_list(ctx.attr.hidden_modules)

  for module in set.to_list(c.modules):
    if not set.is_member(other_modules, module):
      set.mutable_insert(exposed_modules, module)

  conf_file, cache_file = create_ghc_package(
    ctx,
    c.interfaces_dir,
    static_library,
    dynamic_library,
    exposed_modules,
    other_modules,
  )

  return [
    HaskellBuildInfo(
      package_names = set.insert(dep_info.package_names, get_pkg_id(ctx)),
      package_confs = set.insert(dep_info.package_confs, conf_file),
      package_caches = set.insert(dep_info.package_caches, cache_file),
      # NOTE We have to use lists for static libraries because the order is
      # important for linker. Linker searches for unresolved symbols to the
      # left, i.e. you first feed a library which has unresolved symbols and
      # then you feed the library which resolves the symbols.
      static_libraries = [static_library] + dep_info.static_libraries,
      dynamic_libraries = set.insert(dep_info.dynamic_libraries, dynamic_library),
      interface_files = set.union(dep_info.interface_files, set.from_list(c.interface_files)),
      prebuilt_dependencies = dep_info.prebuilt_dependencies,
      external_libraries = dep_info.external_libraries,
    ),
    HaskellLibraryInfo(
      package_name = get_pkg_id(ctx),
      import_dirs = c.import_dirs,
      exposed_modules = exposed_modules,
      other_modules = other_modules,
      haddock_args = c.haddock_args,
      source_files = c.source_files,
    ),
    DefaultInfo(
      files = depset([conf_file, cache_file]),
      runfiles = ctx.runfiles(collect_data = True),
    ),
  ]

haskell_library = rule(
  _haskell_library_impl,
  attrs = dict(
    _haskell_common_attrs,
    hidden_modules = attr.string_list(
      doc = "Modules that should be unavailable for import by dependencies."
    )),
  toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)
"""Build a library from Haskell source.

Example:
  ```bzl
  haskell_library(
      name = 'hello_lib',
      srcs = glob(['hello_lib/**/*.hs']),
      deps = ["//hello_sublib:lib"],
      prebuilt_dependencies = ["base", "bytestring"],
  )
  ```
"""

haskell_doc = _haskell_doc

haskell_lint = _haskell_lint

haskell_toolchain = _haskell_toolchain

ghc_bindist = _ghc_bindist

haskell_repl = _haskell_repl

haskell_cc_import = _haskell_cc_import

cc_haskell_import = _cc_haskell_import
