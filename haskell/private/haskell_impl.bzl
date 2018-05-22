"""Implementation of core Haskell rules"""

load(":private/actions.bzl",
  "compile_haskell_bin",
  "link_haskell_bin",
  "compile_haskell_lib",
  "link_static_lib",
  "link_dynamic_lib",
  "create_ghc_package",
  "gather_dep_info",
  "get_pkg_id",
)
load(":private/set.bzl",
  "set",
)
load(":private/providers.bzl",
  "HaskellBuildInfo",
  "HaskellBinaryInfo",
  "HaskellLibraryInfo",
)
load(":ghci_repl.bzl",
  _build_haskell_repl = "build_haskell_repl",
)

def haskell_binary_impl(ctx):
  c = compile_haskell_bin(ctx)

  binary = link_haskell_bin(ctx, c.object_dyn_files)
  dep_info = gather_dep_info(ctx)

  solibs = set.union(
    set.from_list(dep_info.external_libraries.values()),
    dep_info.dynamic_libraries,
  )

  build_info = dep_info # HaskellBuildInfo
  bin_info = HaskellBinaryInfo(
    source_files = c.source_files,
    modules = c.modules,
    binary = binary,
  )
  target_files = depset([binary])

  _build_haskell_repl(
    ctx,
    build_info=build_info,
    target_files=target_files,
    bin_info=bin_info,
  )

  return [
    build_info,
    bin_info,
    DefaultInfo(
      executable = binary,
      files = target_files,
      runfiles = ctx.runfiles(
        files = set.to_list(solibs),
        collect_data = True,
      ),
    ),
  ]

def haskell_library_impl(ctx):

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

  build_info = HaskellBuildInfo(
    package_ids = set.insert(dep_info.package_ids, get_pkg_id(ctx)),
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
  )
  lib_info = HaskellLibraryInfo(
    package_id = get_pkg_id(ctx),
    import_dirs = c.import_dirs,
    exposed_modules = exposed_modules,
    other_modules = other_modules,
    haddock_args = c.haddock_args,
    source_files = c.source_files,
    header_files = c.header_files,
  )
  target_files = depset([conf_file, cache_file])

  if hasattr(ctx, "outputs"):
    _build_haskell_repl(
      ctx,
      build_info=build_info,
      target_files=target_files,
      lib_info=lib_info,
    )

  default_info = None

  if hasattr(ctx, "runfiles"):
    default_info = DefaultInfo(
      files = target_files,
      runfiles = ctx.runfiles(collect_data = True),
    )
  else:
    default_info = DefaultInfo(
      files = target_files,
    )

  return [
    build_info,
    lib_info,
    default_info,
  ]
