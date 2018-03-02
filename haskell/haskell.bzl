"""Core Haskell rules"""

load(":providers.bzl",
  "HaskellPackageInfo",
  "CcSkylarkApiProviderHacked",
)

load(":actions.bzl",
  "compile_haskell_bin",
  "compile_haskell_lib",
  "create_dynamic_library",
  "create_ghc_package",
  "create_static_library",
  "gather_dependency_information",
  "get_pkg_id",
  "link_haskell_bin",
)

# Re-export haskell_doc
load(":haddock.bzl",
  _haskell_doc = "haskell_doc",
)

# Re-export haskell_toolchain
load(":toolchain.bzl",
  _haskell_toolchain = "haskell_toolchain",
)

# Re-export ghc_bindist
load (":ghc_bindist.bzl",
  _ghc_bindist = "ghc_bindist",
)

load(":cc.bzl",
  _haskell_cc_import = "haskell_cc_import",
  _cc_haskell_import = "cc_haskell_import",
)

load(":set.bzl", "set")

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
  object_files = compile_haskell_bin(ctx)
  return link_haskell_bin(ctx, object_files)

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
      main_function = attr.string(
        default = "Main.main",
        doc = "Location of `main` function.",
      ),
      main_file = attr.label(
        allow_single_file = FileType([".hs", ".hsc", ".lhs"]),
        doc = "File containing `Main` module.",
      )
    ),
    host_fragments = ["cpp"],
    toolchains = [
      "@io_tweag_rules_haskell//haskell:toolchain",
      "@io_tweag_rules_haskell//haskell:binutils-toolchain",
    ],
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
  interfaces_dir, interface_files, object_files, object_dyn_files, haddock_args = compile_haskell_lib(ctx)

  static_library = create_static_library(
    ctx, object_files
  )

  dynamic_library = create_dynamic_library(
    ctx, object_dyn_files
  )

  # Create and register ghc package.
  conf_file, cache_file = create_ghc_package(
    ctx,
    interfaces_dir,
    static_library,
    dynamic_library,
  )

  dep_info = gather_dependency_information(ctx)

  return [HaskellPackageInfo(
    name = dep_info.name,
    # TODO this is somewhat useless now, we shouldn't be abusing
    # HaskellPackageInfo to carry information only relevant during
    # build just to throw it away later as upstream doesn't need this.
    # Technically Haddock rule relies on this but it should gather its
    # own info.
    names = depset(transitive = [dep_info.names, depset([get_pkg_id(ctx)])]),
    confs = depset(transitive = [dep_info.confs, depset([conf_file])]),
    caches = depset(transitive = [dep_info.caches, depset([cache_file])]),
    # We have to use lists for static libraries because the order is
    # important for linker. Linker searches for unresolved symbols to the
    # left, i.e. you first feed a library which has unresolved symbols and
    # then you feed the library which resolves the symbols.
    static_libraries = [static_library] + dep_info.static_libraries,
    dynamic_libraries = set.insert(dep_info.dynamic_libraries, dynamic_library),
    interface_files = set.union(dep_info.interface_files, set.from_list(interface_files)),
    prebuilt_dependencies = set.union(
      dep_info.prebuilt_dependencies,
      set.from_list(ctx.attr.prebuilt_dependencies)
    ),
    external_libraries = dep_info.external_libraries,
    haddock_ghc_args = haddock_args,
  ),
  DefaultInfo(files = depset([
      conf_file,
      cache_file,
  ])),
  ]

haskell_library = rule(
  _haskell_library_impl,
  attrs = dict(
    _haskell_common_attrs,
    hidden_modules = attr.string_list(
      doc = "Modules that should be made unavailable for import by dependencies."
    )),
  host_fragments = ["cpp"],
  toolchains = [
    "@io_tweag_rules_haskell//haskell:toolchain",
    "@io_tweag_rules_haskell//haskell:binutils-toolchain",
  ],
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

haskell_toolchain = _haskell_toolchain

ghc_bindist = _ghc_bindist

haskell_cc_import = _haskell_cc_import

cc_haskell_import = _cc_haskell_import
