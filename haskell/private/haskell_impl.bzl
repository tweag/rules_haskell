"""Implementation of core Haskell rules"""

load(":cc.bzl", "cc_interop_info")
load(":private/context.bzl", "haskell_context")
load(":private/actions/compile.bzl", "compile_library")
load(":private/actions/link.bzl",
  "link_binary",
  "link_library_dynamic",
  "link_library_static",
)
load(":private/actions/package.bzl", "package")
load(":private/pkg_id.bzl", "pkg_id")
load(":private/actions/repl.bzl", "build_haskell_repl")
load(":private/dependencies.bzl", "gather_dep_info")
load(":private/java.bzl", "java_interop_info")
load(":private/set.bzl", "set")
load(":private/mode.bzl", "is_profiling_enabled")
load(":private/providers.bzl",
  "HaskellBuildInfo",
  "HaskellBinaryInfo",
  "HaskellLibraryInfo",
  "C2hsLibraryInfo",
)

def _prepare_srcs(srcs):

  srcs_files = []
  import_dir_map = {}

  for src in srcs:
    # If it has the "files" attribute, it must be a Target
    if hasattr(src, "files"):
      if C2hsLibraryInfo in src:
        srcs_files += src.files.to_list()
        for f in src.files:
          import_dir_map[f] = src[C2hsLibraryInfo].import_dir
      else:
        srcs_files += src.files.to_list()
    # otherwise it's just a file
    else:
      srcs_files.append(src)

  return srcs_files, import_dir_map

def haskell_binary_impl(ctx):
  hs = haskell_context(ctx)
  dep_info = gather_dep_info(ctx)

  # Add any interop info for other languages.
  cc = cc_interop_info(ctx)
  java = java_interop_info(ctx)
  with_profiling = is_profiling_enabled(hs)

  srcs_files, import_dir_map = _prepare_srcs(ctx.attr.srcs)

  c = hs.toolchain.actions.compile_binary(
    hs,
    cc,
    java,
    dep_info,
    srcs = srcs_files,
    import_dir_map = import_dir_map,
    extra_srcs = depset(ctx.files.extra_srcs),
    compiler_flags = ctx.attr.compiler_flags,
    with_profiling = False,
    main_file = ctx.file.main_file,
    main_function = ctx.attr.main_function,
  )

  c_p = None

  if with_profiling:
    c_p = hs.toolchain.actions.compile_binary(
      hs,
      cc,
      java,
      dep_info,
      srcs = srcs_files,
      import_dir_map = import_dir_map,
      # NOTE We must make the object files compiled without profiling
      # available to this step for TH to work, presumably because GHC is
      # linked against RTS without profiling.
      extra_srcs = depset(transitive = [
        depset(ctx.files.extra_srcs),
        depset(c.object_dyn_files),
      ]),
      compiler_flags = ctx.attr.compiler_flags,
      with_profiling = True,
      main_file = ctx.file.main_file,
      main_function = ctx.attr.main_function,
    )

  binary = link_binary(
    hs,
    cc,
    dep_info,
    ctx.files.extra_srcs,
    ctx.attr.compiler_flags,
    c_p.object_files if with_profiling else c.object_dyn_files,
    with_profiling,
  )

  solibs = set.union(
    set.from_list(dep_info.external_libraries.values()),
    dep_info.dynamic_libraries,
  )

  build_info = dep_info # HaskellBuildInfo
  bin_info = HaskellBinaryInfo(
    import_dirs = c.import_dirs,
    source_files = c.source_files,
    modules = c.modules,
    binary = binary,
    ghc_args = c.ghc_args,
    header_files = c.header_files,
  )
  target_files = depset([binary])

  build_haskell_repl(
    hs,
    ghci_script = ctx.file._ghci_script,
    ghci_repl_wrapper = ctx.file._ghci_repl_wrapper,
    compiler_flags = ctx.attr.compiler_flags,
    repl_ghci_args = ctx.attr.repl_ghci_args,
    output = ctx.outputs.repl,
    interpreted = ctx.attr.repl_interpreted,
    build_info = build_info,
    target_files = target_files,
    bin_info = bin_info,
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
  hs = haskell_context(ctx)
  dep_info = gather_dep_info(ctx)
  my_pkg_id = pkg_id.new(ctx.label, ctx.attr.version)
  with_profiling = is_profiling_enabled(hs)

  # Add any interop info for other languages.
  cc = cc_interop_info(ctx)
  java = java_interop_info(ctx)

  srcs_files, import_dir_map = _prepare_srcs(ctx.attr.srcs)

  c = hs.toolchain.actions.compile_library(
    hs,
    cc,
    java,
    dep_info,
    srcs = srcs_files,
    import_dir_map = import_dir_map,
    extra_srcs = depset(ctx.files.extra_srcs),
    compiler_flags = ctx.attr.compiler_flags,
    with_profiling = False,
    my_pkg_id = my_pkg_id,
  )

  c_p = None

  if with_profiling:
    c_p = hs.toolchain.actions.compile_library(
      hs,
      cc,
      java,
      dep_info,
      srcs = srcs_files,
      import_dir_map = import_dir_map,
      # NOTE We must make the object files compiled without profiling
      # available to this step for TH to work, presumably because GHC is
      # linked against RTS without profiling.
      extra_srcs = depset(transitive = [
        depset(ctx.files.extra_srcs),
        depset(c.object_dyn_files),
      ]),
      compiler_flags = ctx.attr.compiler_flags,
      with_profiling = True,
      my_pkg_id = my_pkg_id,
    )

  static_library = link_library_static(
    hs,
    cc,
    dep_info,
    c.object_files,
    my_pkg_id,
    with_profiling = False,
  )
  dynamic_library = link_library_dynamic(
    hs,
    cc,
    dep_info,
    ctx.files.extra_srcs,
    c.object_dyn_files,
    my_pkg_id,
  )

  static_library_prof = None
  if with_profiling:
    static_library_prof = link_library_static(
      hs,
      cc,
      dep_info,
      c_p.object_files,
      my_pkg_id,
      with_profiling = True,
    )

  exposed_modules = set.empty()
  other_modules = set.from_list(ctx.attr.hidden_modules)

  for module in set.to_list(c.modules):
    if not set.is_member(other_modules, module):
      set.mutable_insert(exposed_modules, module)

  conf_file, cache_file = package(
    hs,
    dep_info,
    c.interfaces_dir,
    static_library,
    dynamic_library,
    exposed_modules,
    other_modules,
    my_pkg_id,
    static_library_prof = static_library_prof,
    interface_files = c.interface_files,
  )

  static_libraries_prof = dep_info.static_libraries_prof

  if static_library_prof != None:
    static_libraries_prof = [static_library_prof] + dep_info.static_libraries_prof

  interface_files = set.union(
    dep_info.interface_files,
    set.from_list(c.interface_files),
  )

  if c_p != None:
    interface_files = set.mutable_union(
      interface_files,
      set.from_list(c_p.interface_files),
    )

  build_info = HaskellBuildInfo(
    package_ids = set.insert(dep_info.package_ids, pkg_id.to_string(my_pkg_id)),
    package_confs = set.insert(dep_info.package_confs, conf_file),
    package_caches = set.insert(dep_info.package_caches, cache_file),
    # NOTE We have to use lists for static libraries because the order is
    # important for linker. Linker searches for unresolved symbols to the
    # left, i.e. you first feed a library which has unresolved symbols and
    # then you feed the library which resolves the symbols.
    static_libraries = [static_library] + dep_info.static_libraries,
    static_libraries_prof = static_libraries_prof,
    dynamic_libraries = set.insert(dep_info.dynamic_libraries, dynamic_library),
    interface_files = interface_files,
    prebuilt_dependencies = dep_info.prebuilt_dependencies,
    external_libraries = dep_info.external_libraries,
  )
  lib_info = HaskellLibraryInfo(
    package_id = pkg_id.to_string(my_pkg_id),
    version = ctx.attr.version,
    import_dirs = c.import_dirs,
    exposed_modules = exposed_modules,
    other_modules = other_modules,
    ghc_args = c.ghc_args,
    header_files = c.header_files,
    boot_files = c.boot_files,
    source_files = c.source_files,
    extra_source_files = c.extra_source_files,
  )
  target_files = depset([conf_file, cache_file])

  if hasattr(ctx, "outputs"):
    build_haskell_repl(
      hs,
      ghci_script = ctx.file._ghci_script,
      ghci_repl_wrapper = ctx.file._ghci_repl_wrapper,
      repl_ghci_args = ctx.attr.repl_ghci_args,
      compiler_flags = ctx.attr.compiler_flags,
      output = ctx.outputs.repl,
      interpreted = ctx.attr.repl_interpreted,
      build_info = build_info,
      target_files = target_files,
      lib_info = lib_info,
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
