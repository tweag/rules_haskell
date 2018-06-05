"""Implementation of core Haskell rules"""

load(":cc.bzl", "cc_headers")
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
load(":private/providers.bzl",
  "HaskellBuildInfo",
  "HaskellBinaryInfo",
  "HaskellLibraryInfo",
)

def haskell_binary_impl(ctx):
  hs = haskell_context(ctx)
  dep_info = gather_dep_info(ctx)

  # Add any interop info for other languages.
  cc = cc_headers(ctx)
  java = java_interop_info(ctx)

  c = hs.toolchain.actions.compile_binary(
    hs,
    cc,
    java,
    dep_info,
    cpp_defines = ctx.file._cpp_defines,
    compiler_flags = ctx.attr.compiler_flags,
    srcs = ctx.files.srcs,
    extra_srcs = depset(ctx.files.extra_srcs),
    main_file = ctx.file.main_file,
    main_function = ctx.attr.main_function,
  )

  binary = link_binary(hs, dep_info, ctx.attr.compiler_flags, c.object_dyn_files)

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

  build_haskell_repl(
    hs,
    ghci_script = ctx.file._ghci_script,
    ghci_repl_wrapper = ctx.file._ghci_repl_wrapper,
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

  # Add any interop info for other languages.
  cc = cc_headers(ctx)
  java = java_interop_info(ctx)

  c = hs.toolchain.actions.compile_library(
    hs,
    cc,
    java,
    dep_info,
    cpp_defines = ctx.file._cpp_defines,
    compiler_flags = ctx.attr.compiler_flags,
    srcs = ctx.files.srcs,
    extra_srcs = depset(ctx.files.extra_srcs),
    my_pkg_id = my_pkg_id,
  )

  static_library = link_library_static(
    hs,
    dep_info,
    c.object_files,
    my_pkg_id,
  )
  dynamic_library = link_library_dynamic(
    hs,
    dep_info,
    c.object_dyn_files,
    my_pkg_id,
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
    dynamic_libraries = set.insert(dep_info.dynamic_libraries, dynamic_library),
    interface_files = set.union(dep_info.interface_files, set.from_list(c.interface_files)),
    prebuilt_dependencies = dep_info.prebuilt_dependencies,
    external_libraries = dep_info.external_libraries,
  )
  lib_info = HaskellLibraryInfo(
    package_id = pkg_id.to_string(my_pkg_id),
    version = ctx.attr.version,
    import_dirs = c.import_dirs,
    exposed_modules = exposed_modules,
    other_modules = other_modules,
    haddock_args = c.haddock_args,
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
