"""Entry point to rules_haskell.
"""
load(":toolchain.bzl",
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

load(":hsc2hs.bzl",
     "hsc_to_hs",
)

load(":c_compile.bzl",
     "c_compile_dynamic",
     "c_compile_static",
)

def _haskell_binary_impl(ctx):
  object_files = compile_haskell_bin(ctx)
  link_haskell_bin(ctx, object_files)

def _haskell_library_impl(ctx):
  # Process hsc files
  processed_hsc_files = hsc_to_hs(ctx)

  interfaces_dir, interface_files, object_files, object_dyn_files = compile_haskell_lib(
    ctx, processed_hsc_files
  )

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
    interface_files, interfaces_dir,
    static_library, static_library_dir,
    dynamic_library_dir
  )

  dep_info = gather_dependency_information(ctx)
  return [HaskellPackageInfo(
    name = dep_info.name,
    names = dep_info.names + depset([get_pkg_id(ctx)]), #depPkgNames + depset([get_pkg_id(ctx)]),
    confs = dep_info.confs + depset([conf_file]),
    caches = dep_info.caches + depset([cache_file]),
    # Keep package libraries in preorder (naive_link) order: this
    # gives us the valid linking order at binary linking time.
    static_libraries = depset([static_library], order="preorder") + dep_info.static_libraries,
    dynamic_libraries = depset([dynamic_library]) + dep_info.dynamic_libraries,
    interface_files = dep_info.interface_files + depset(interface_files),
    static_library_dirs = dep_info.static_library_dirs + depset([static_library_dir]),
    dynamic_library_dirs = dep_info.dynamic_library_dirs + depset([dynamic_library_dir]),
    prebuilt_dependencies = dep_info.prebuilt_dependencies + depset(ctx.attr.prebuilt_dependencies),
    external_libraries = dep_info.external_libraries
  )]

_haskell_common_attrs = {
  "src_strip_prefix": attr.string(
    mandatory=False,
    doc="Directory in which module hierarchy starts."
  ),
  "srcs": attr.label_list(
    allow_files=FileType([".hs"]),
    # TODO: Figure out how to deal with sources where module hierarchy
    # doesn't start straight away.
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
  "compilerFlags": attr.string_list(
    doc="Flags to pass to Haskell compiler while compiling this rule's sources."
  ),
  "hscs": attr.label_list(
    allow_files=FileType([".hsc"]),
    doc=".hsc files to preprocess and link"
  ),
  "external_deps": attr.label_list(
    allow_files=True,
    doc="Non-Haskell dependencies",
  ),
  # Only supports one per lib for now
  "external_libraries": attr.string_dict(
    doc="Non-Haskell libraries that we should link",
  ),
  "prebuilt_dependencies": attr.string_list(
    doc="Haskell packages which are magically available such as wired-in packages."
  ),
  "version": attr.string(
    default="1.0.0",
    doc="Package/binary version"
  ),
  "ghcVersion": attr.string(
    default="8.2.2",
    # TODO (fuuzetsu): We need this because we have to generate
    # correct suffix for shared libraries that GHC expects for
    # dynamic-library-dirs content. As currently we're using GHC from
    # nix, there's not really a way to do this. In future we need to
    # expose toolchains that expose a version and use that. I think.
    doc="Version of GHC used."
  ),
}

haskell_library = rule(
  _haskell_library_impl,
  outputs = {
    "conf": "%{name}-%{version}/%{name}-%{version}.conf",
    "packageCache": "%{name}-%{version}/package.cache"
  },
  attrs = _haskell_common_attrs,
)

haskell_binary = rule(
  _haskell_binary_impl,
  executable = True,
  attrs = _haskell_common_attrs + {
    "main": attr.string(
      default="Main.main",
      doc="Main function location."
    )
  }
)
