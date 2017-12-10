"""Entry point to rules_haskell.
"""
load(":toolchain.bzl",
     "HaskellPackageInfo",
     "get_dyn_interface_suffix",
     "get_dyn_object_suffix",
     "get_interface_suffix",
     "get_object_suffix",
     "link_haskell_bin",
     "create_dynamic_library",
     "mk_name",
     "path_append",
     "replace_ext",
     "compile_haskell_lib",
     "get_input_files",
     "create_static_library",
     "get_pkg_id",
     "create_ghc_package",
     "compile_haskell_bin",
)

load(":path_utils.bzl",
     "declare_compiled",
)

load(":hsc2hs.bzl",
     "hsc_to_hs",
)

load(":cpphs.bzl",
     "cpphs",
)

load(":c_compile.bzl",
     "c_compile_dynamic",
     "c_compile_static",
)

def _haskell_binary_impl(ctx):
  externalLibs = {}
  for d in ctx.attr.deps:
    externalLibs.update(d[HaskellPackageInfo].externalLibs)

  object_files = compile_haskell_bin(ctx)

  link_haskell_bin(ctx, object_files, externalLibs)

def _haskell_library_impl(ctx):
  objects_dir = ctx.actions.declare_directory(mk_name(ctx, "objects"))

  # Directory used for interface files.
  interfaces_dir = ctx.actions.declare_directory(mk_name(ctx, "interfaces"))

  # Build transitive depsets
  depPkgConfs = depset()
  depPkgCaches = depset()
  depPkgNames = depset()
  depPkgLibs = depset()
  depPkgDynLibs = depset()
  depInterfaceFiles = depset()
  depImportDirs = depset()
  depLibDirs = depset()
  depDynLibDirs = depset()
  depPrebuiltDeps = depset()
  allExternalLibs = {}
  for d in ctx.attr.deps:
    pkg = d[HaskellPackageInfo]
    depPkgConfs += pkg.pkgConfs
    depPkgCaches += pkg.pkgCaches
    depPkgNames += pkg.pkgNames
    depInterfaceFiles += pkg.interfaceFiles
    depPkgLibs += pkg.pkgLibs
    depPkgDynLibs += pkg.pkgDynLibs
    depImportDirs += pkg.pkgImportDirs
    depLibDirs += pkg.pkgLibDirs
    depDynLibDirs += pkg.pkgDynLibDirs
    depPrebuiltDeps += pkg.prebuiltDeps
    allExternalLibs.update(pkg.externalLibs)

  # Put external libraries for current target into the external lib
  # dict.
  for name, f in ctx.attr.external_libraries.items():
    allExternalLibs[name] = allExternalLibs.get(name, default=depset()) + [f]

  # Process hsc files
  processed_hsc_files = hsc_to_hs(ctx)
  # Process cpphs files
  processed_cpphs_files = cpphs(ctx)

  interfaces_dir, interface_files, object_files, object_dyn_files = compile_haskell_lib(
    ctx, processed_hsc_files + processed_cpphs_files
  )

  c_object_files = c_compile_static(ctx)
  static_library_dir, static_library = create_static_library(
    ctx, object_files + c_object_files
  )

  c_object_dyn_files = c_compile_dynamic(ctx)
  dynamic_library_dir, dynamic_library = create_dynamic_library(
    ctx, allExternalLibs, object_dyn_files + c_object_dyn_files
  )

  # Create and register ghc package.
  conf_file, cache_file = create_ghc_package(
    ctx,
    interface_files, interfaces_dir,
    static_library, static_library_dir,
    dynamic_library_dir
  )

  return [HaskellPackageInfo(
    pkgName = get_pkg_id(ctx),
    pkgNames = depPkgNames + depset([get_pkg_id(ctx)]),
    pkgConfs = depPkgConfs + depset ([conf_file]),
    pkgCaches = depPkgCaches + depset([cache_file]),
    # Keep package libraries in preorder (naive_link) order: this
    # gives us the valid linking order at binary linking time.
    pkgLibs = depset([static_library], order="preorder") + depPkgLibs,
    pkgDynLibs = depset([dynamic_library]) + depPkgDynLibs,
    interfaceFiles = depInterfaceFiles + depset(interface_files),
    pkgImportDirs = depImportDirs + depset([interfaces_dir]),
    pkgLibDirs = depLibDirs + depset([static_library_dir]),
    pkgDynLibDirs = depLibDirs + depset([dynamic_library_dir]),
    prebuiltDeps = depPrebuiltDeps + depset(ctx.attr.prebuiltDeps),
    externalLibs = allExternalLibs
  )]

_haskell_common_attrs = {
  "sourceDir": attr.string(
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
  "cpphs": attr.label_list(
    allow_files=FileType([".cpphs"]),
    doc=".cpphs file to preprocess and link",
  ),
  "external_deps": attr.label_list(
    allow_files=True,
    doc="Non-Haskell dependencies",
  ),
  # Only supports one per lib for now
  "external_libraries": attr.string_dict(
    doc="Non-Haskell libraries that we should link",
  ),
  "prebuiltDeps": attr.string_list(
    doc="Haskell packages which are magically available such as wired-in packages."
  ),
  "version": attr.string(
    default="1.0.0",
    doc="Package/binary version"
  ),
  "ghcVersion": attr.string(
    default="8.0.2",
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
