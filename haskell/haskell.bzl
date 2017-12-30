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

load(":c_compile.bzl",
  "c_compile_dynamic",
  "c_compile_static",
)

# Re-export haskell_haddock
load (":haddock.bzl",
  _haskell_haddock = "haskell_haddock",
)

# Re-export haskell_toolchain
load (":toolchain.bzl",
  _haskell_toolchain = "haskell_toolchain",
)

_haskell_common_attrs = {
  "src_strip_prefix": attr.string(
    mandatory=False,
    doc="Directory in which module hierarchy starts."
  ),
  "srcs": attr.label_list(
    allow_files=FileType([".hs", ".hsc"]),
    doc="A list of Haskell sources to be built by this rule."
  ),
  "c_sources": attr.label_list(
    allow_files=FileType([".c"]),
    doc="A list of C source files to be built as part of the package."
  ),
  "c_options": attr.string_list(
    doc="Options to pass to C compiler for any C source files."
  ),
  "deps": attr.label_list(
    doc="haskell_library dependencies"
  ),
  "compiler_flags": attr.string_list(
    doc="Flags to pass to Haskell compiler while compiling this rule's sources."
  ),
  "external_deps": attr.label_list(
    allow_files=True,
    doc="Non-Haskell dependencies",
  ),
  "prebuilt_dependencies": attr.string_list(
    doc="Haskell packages which are magically available such as wired-in packages."
  ),
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
    attrs = dict(_haskell_common_attrs,
      main = attr.string(
        default="Main.main",
        doc="Main function location.",
      ),
    ),
    host_fragments = ["cpp"],
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
    **kwargs
  )

haskell_test = _mk_binary_rule(test = True)

haskell_binary = _mk_binary_rule()

def _haskell_library_impl(ctx):
  interfaces_dir, interface_files, object_files, object_dyn_files = compile_haskell_lib(ctx)

  c_object_files = c_compile_static(ctx)
  static_library_dir, static_library = create_static_library(
    ctx, object_files + c_object_files
  )

  c_object_dyn_files = c_compile_dynamic(ctx)
  dynamic_library_dir, dynamic_library = create_dynamic_library(
    ctx, object_dyn_files + c_object_dyn_files
  )

  # Create and register ghc package.
  conf_file, cache_file = create_ghc_package(
    ctx,
    interfaces_dir,
    static_library,
    dynamic_library,
    static_library_dir,
    dynamic_library_dir,
  )

  dep_info = gather_dependency_information(ctx)
  return [HaskellPackageInfo(
    name = dep_info.name,
    names = depset(transitive = [dep_info.names, depset([get_pkg_id(ctx)])]),
    confs = depset(transitive = [dep_info.confs, depset([conf_file])]),
    caches = depset(transitive = [dep_info.caches, depset([cache_file])]),
    # Keep package libraries in preorder (naive_link) order: this
    # gives us the valid linking order at binary linking time.
    static_libraries = depset(
      transitive = [depset([static_library]), dep_info.static_libraries],
      order = "preorder"
    ),
    dynamic_libraries = depset(
      transitive = [depset([dynamic_library]), dep_info.dynamic_libraries]
    ),
    interface_files = depset(
      transitive = [dep_info.interface_files, depset(interface_files)]
    ),
    static_library_dirs = depset(
      transitive = [dep_info.static_library_dirs, depset([static_library_dir])]
    ),
    dynamic_library_dirs = depset(
      transitive = [dep_info.dynamic_library_dirs, depset([dynamic_library_dir])]
    ),
    prebuilt_dependencies = depset(
      transitive = [
        dep_info.prebuilt_dependencies,
        depset(ctx.attr.prebuilt_dependencies),
      ]
    ),
    external_libraries = dep_info.external_libraries
  )]

haskell_library = rule(
  _haskell_library_impl,
  outputs = {
    "conf": "%{name}-%{version}/%{name}-%{version}.conf",
    "package_cache": "%{name}-%{version}/package.cache"
  },
  attrs = _haskell_common_attrs,
  host_fragments = ["cpp"],
  toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)

haskell_haddock = _haskell_haddock

haskell_toolchain = _haskell_toolchain

def haskell_import(name, shared_library, visibility = None):
  native.alias(name = name, actual = shared_library, visibility = visibility)
