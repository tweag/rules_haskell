"""Entry point to rules_haskell."""

load(":actions.bzl",
  "HaskellPackageInfo",
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
load (":haddock.bzl",
  _haskell_doc = "haskell_doc",
)

# Re-export haskell_toolchain
load (":toolchain.bzl",
  _haskell_toolchain = "haskell_toolchain",
)

load(":cc.bzl",
  "CcSkylarkApiProviderHacked",
  _haskell_cc_import = "haskell_cc_import",
  _cc_haskell_import = "cc_haskell_import",
)

load(":set.bzl", "set")

_haskell_common_attrs = {
  "src_strip_prefix": attr.string(default="",mandatory=False),
  "srcs": attr.label_list(allow_files=FileType(
    [".hs", ".hsc", ".lhs", ".hs-boot", ".lhs-boot", ".h"]
  )),
  "copts": attr.string_list(),
  "deps": attr.label_list(),
  "compiler_flags": attr.string_list(),
  "prebuilt_dependencies": attr.string_list(),
  # XXX Consider making this private. Blocked on
  # https://github.com/bazelbuild/bazel/issues/4366.
  "version": attr.string(
    default="1.0.0",
    doc="Package/binary version"
  ),
}

def _haskell_binary_impl(ctx):
  object_files = compile_haskell_bin(ctx)
  link_haskell_bin(ctx, object_files)

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
      main_function = attr.string(default="Main.main"),
      main_file = attr.label(
        allow_single_file=FileType([".hs", ".hsc", ".lhs"]),
      )
    ),
    host_fragments = ["cpp"],
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
    **kwargs
  )

haskell_test = _mk_binary_rule(test = True)

haskell_binary = _mk_binary_rule()

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
    static_libraries = set.insert(dep_info.static_libraries, static_library),
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
  attrs = _haskell_common_attrs,
  host_fragments = ["cpp"],
  toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)

haskell_doc = _haskell_doc

haskell_toolchain = _haskell_toolchain

haskell_cc_import = _haskell_cc_import

cc_haskell_import = _cc_haskell_import
