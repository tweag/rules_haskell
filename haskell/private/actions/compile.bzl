"""Actions for compiling Haskell source code"""

load(":cc.bzl", "cc_headers")
load(":private/actions/package.bzl", "get_pkg_id")
load(":private/java.bzl", "java_interop_info")
load(":private/path_utils.bzl",
  "declare_compiled",
  "target_unique_name",
  "module_name",
  "module_unique_name",
  "import_hierarchy_root",
  "get_external_libs_path",
)
load(":private/mode.bzl", "add_mode_options")
load(":private/providers.bzl", "DefaultCompileInfo")
load(":private/set.bzl", "set")
load(":private/tools.bzl",
  "get_build_tools_path",
  "tools",
)
load("@bazel_skylib//:lib.bzl", "dicts", "paths")

def _make_ghc_defs_dump(ctx):
  """Generate a file containing GHC default pre-processor definitions.

  Args:
    ctx: Rule context.

  Returns:
    File: The file with GHC definitions.
  """
  raw_filename = "ghc-defs-dump-{0}-{1}.hs".format(ctx.attr.name, ctx.attr.version)
  dummy_src = ctx.actions.declare_file(raw_filename)
  ghc_defs_dump_raw = ctx.actions.declare_file(paths.replace_extension(raw_filename, ".hspp"))
  ghc_defs_dump = ctx.actions.declare_file(paths.replace_extension(raw_filename, ".h"))

  ctx.actions.write(dummy_src, "")
  args = ctx.actions.args()
  args.add([
    "-E",
    "-optP-dM",
    "-cpp",
    dummy_src.path,
  ])

  ctx.actions.run(
    inputs = [dummy_src],
    outputs = [ghc_defs_dump_raw],
    executable = tools(ctx).ghc,
    arguments = [args],
  )

  ctx.actions.run_shell(
    inputs = [ghc_defs_dump_raw, ctx.file._cpp_defines, tools(ctx).grep],
    outputs = [ghc_defs_dump],
    command = """
    grep "^[^#]" {cpp_defines} | while IFS= read -r patt; do
      grep "$patt" {raw} >> {filtered}
    done
    """.format(
      cpp_defines = ctx.file._cpp_defines.path,
      raw = ghc_defs_dump_raw.path,
      filtered = ghc_defs_dump.path,
    ),
    env = {
      "PATH": get_build_tools_path(ctx),
    },
  )

  return ghc_defs_dump

def _process_hsc_file(ctx, ghc_defs_dump, hsc_file):
  """Process a single hsc file.

  Args:
    ctx: Rule context.
    ghc_defs_dump: File with GHC definitions.
    hsc_file: hsc file to process.

  Returns:
    File: Haskell source file created by processing hsc_file.
  """

  hsc_output_dir = ctx.actions.declare_directory(
    module_unique_name(ctx, hsc_file, "hsc_processed")
  )
  args = ctx.actions.args()

  # Output a Haskell source file.
  hs_out = declare_compiled(ctx, hsc_file, ".hs", directory=hsc_output_dir)
  args.add([hsc_file, "-o", hs_out])

  # Bring in scope the header files of dependencies, if any.
  cc = cc_headers(ctx)
  args.add(["--cflag=" + f for f in cc.cpp_flags])
  args.add(["--cflag=" + f for f in cc.include_args])
  args.add("-I{0}".format(ghc_defs_dump.dirname))
  args.add("-i{0}".format(ghc_defs_dump.basename))

  ctx.actions.run(
    inputs = depset(transitive = [
      depset(cc.hdrs),
      depset([tools(ctx).gcc]),
      depset([hsc_file, ghc_defs_dump])
    ]),
    outputs = [hs_out, hsc_output_dir],
    progress_message = "hsc2hs {0}".format(hsc_file.basename),
    env = {
      "PATH": get_build_tools_path(ctx),
    },
    executable = tools(ctx).hsc2hs,
    arguments = [args],
  )
  return hs_out

def _compilation_defaults(ctx, dep_info):
  """Declare default compilation targets and create default compiler arguments.

  Args:
    ctx: Rule context.

  Returns:
    DefaultCompileInfo: Populated default compilation settings.
  """
  args = ctx.actions.args()
  haddock_args = ctx.actions.args()

  # Declare file directories
  objects_dir_raw = target_unique_name(ctx, "objects")
  objects_dir = ctx.actions.declare_directory(objects_dir_raw)
  interfaces_dir_raw = target_unique_name(ctx, "interfaces")
  interfaces_dir = ctx.actions.declare_directory(interfaces_dir_raw)

  # Compilation mode and explicit user flags
  if ctx.var["COMPILATION_MODE"] == "opt":
    args.add("-O2")

  args.add(ctx.attr.compiler_flags)
  haddock_args.add(ctx.attr.compiler_flags, before_each="--optghc")

  # Output static and dynamic object files.
  args.add(["-static", "-dynamic-too"])

  # Common flags
  args.add([
    "-v0",
    "-c",
    "--make",
    "-fPIC",
    "-hide-all-packages",
  ])
  haddock_args.add(["-hide-all-packages"], before_each="--optghc")
  haddock_args.add(["-v0"])

  args.add([
    "-odir", objects_dir,
    "-hidir", interfaces_dir,
  ])

  add_mode_options(ctx, args)

  # Add import hierarchy root.
  # Note that this is not perfect, since GHC requires hs-boot files
  # to be in the same directory as the corresponding .hs file.  Thus
  # the two must both have the same root; i.e., both plain files,
  # both in bin_dir, or both in genfiles_dir.
  import_root = import_hierarchy_root(ctx)
  ih_root_arg = ["-i{0}".format(import_root),
                 "-i{0}".format(paths.join(ctx.bin_dir.path, import_root)),
                 "-i{0}".format(paths.join(ctx.genfiles_dir.path, import_root))]
  args.add(ih_root_arg)
  haddock_args.add(ih_root_arg, before_each="--optghc")

  # Expose all prebuilt dependencies
  for prebuilt_dep in ctx.attr.prebuilt_dependencies:
    items = ["-package", prebuilt_dep]
    args.add(items)
    haddock_args.add(items, before_each="--optghc")

  # Expose all bazel dependencies
  for package in set.to_list(dep_info.package_ids):
    items = ["-package-id", package]
    args.add(items)
    if package != get_pkg_id(ctx):
      haddock_args.add(items, before_each="--optghc")

  # Only include package DBs for deps, prebuilt deps should be found
  # auto-magically by GHC.
  for cache in set.to_list(dep_info.package_caches):
    items = ["-package-db", cache.dirname]
    args.add(items)
    haddock_args.add(items, before_each="--optghc")

  # We want object and dynamic objects from all inputs.
  object_files = []
  object_dyn_files = []

  # We need to keep interface files we produce so we can import
  # modules cross-package.
  interface_files = []
  other_sources = []
  modules = set.empty()
  source_files = set.empty()
  import_dirs = set.singleton(import_root)

  # Output object files are named after modules, not after input file names.
  # The difference is only visible in the case of Main module because it may
  # be placed in a file with a name different from "Main.hs". In that case
  # still Main.o will be produced.

  ghc_defs_dump = _make_ghc_defs_dump(ctx)

  for s in ctx.files.srcs:

    if s.extension in ["h", "hs-boot", "lhs-boot"]:
      other_sources.append(s)
    elif s.extension in ["hs", "lhs", "hsc"]:
      if not hasattr(ctx.file, "main_file") or (s != ctx.file.main_file):
        if s.extension == "hsc":
          s0 = _process_hsc_file(ctx, ghc_defs_dump, s)
          set.mutable_insert(source_files, s0)
        else:
          set.mutable_insert(source_files, s)
        set.mutable_insert(modules, module_name(ctx, s))
        object_files.append(
          declare_compiled(ctx, s, ".o", directory=objects_dir)
        )
        object_dyn_files.append(
          declare_compiled(ctx, s, ".dyn_o", directory=objects_dir)
        )
        interface_files.append(
          declare_compiled(ctx, s, ".hi", directory=interfaces_dir)
        )
        interface_files.append(
          declare_compiled(ctx, s, ".dyn_hi", directory=interfaces_dir)
        )
      else:
        if s.extension == "hsc":
          s0 = _process_hsc_file(ctx, ghc_defs_dump, s)
          set.mutable_insert(source_files, s0)
        else:
          set.mutable_insert(source_files, s)
        set.mutable_insert(modules, "Main")
        object_files.append(
          ctx.actions.declare_file(paths.join(objects_dir_raw, "Main.o"))
        )
        object_dyn_files.append(
          ctx.actions.declare_file(paths.join(objects_dir_raw, "Main.dyn_o"))
        )
        interface_files.append(
          ctx.actions.declare_file(paths.join(interfaces_dir_raw, "Main.hi"))
        )
        interface_files.append(
          ctx.actions.declare_file(paths.join(interfaces_dir_raw, "Main.dyn_hi"))
        )

  cc = cc_headers(ctx)
  preprocessor_args = ["-optP" + f for f in cc.cpp_flags]
  args.add(preprocessor_args)
  args.add(cc.include_args)
  haddock_args.add(preprocessor_args, before_each="--optghc")
  haddock_args.add(cc.include_args, before_each="--optghc")

  for f in set.to_list(source_files):
    args.add(f)
    haddock_args.add(f)

  # Add any interop info for other languages.
  java = java_interop_info(ctx)

  return DefaultCompileInfo(
    args = args,
    haddock_args = haddock_args,
    inputs = depset(transitive = [
      set.to_depset(source_files),
      depset(cc.hdrs),
      set.to_depset(dep_info.package_confs),
      set.to_depset(dep_info.package_caches),
      set.to_depset(dep_info.interface_files),
      set.to_depset(dep_info.dynamic_libraries),
      depset(dep_info.external_libraries.values()),
      java.inputs,
      depset(other_sources),
      depset([tools(ctx).gcc]),
    ]),
    outputs = [objects_dir, interfaces_dir] + object_files + object_dyn_files + interface_files,
    objects_dir = objects_dir,
    interfaces_dir = interfaces_dir,
    object_files = object_files,
    object_dyn_files = object_dyn_files,
    interface_files = interface_files,
    modules = modules,
    source_files = source_files,
    header_files = set.from_list(cc.hdrs),
    import_dirs = import_dirs,
    env = dicts.add({
      "LD_LIBRARY_PATH": get_external_libs_path(set.from_list(dep_info.external_libraries.values())),
      },
      java.env,
    ),
  )

def compile_haskell_bin(ctx, dep_info):
  """Compile a Haskell target into object files suitable for linking.

  Args:
    ctx: Rule context.

  Returns:
    struct with the following fields:
      object_files: list of static object files
      object_dyn_files: list of dynamic object files
      modules: set of module names
      source_files: set of Haskell source files
  """
  c = _compilation_defaults(ctx, dep_info)
  c.args.add(["-main-is", ctx.attr.main_function])

  ctx.actions.run(
    inputs = c.inputs,
    outputs = c.outputs,
    progress_message = "Building {0}".format(ctx.attr.name),
    env = c.env,
    executable = tools(ctx).ghc,
    arguments = [c.args]
  )

  return struct(
    object_files = c.object_files,
    object_dyn_files = c.object_dyn_files,
    modules = c.modules,
    source_files = c.source_files,
  )

def compile_haskell_lib(ctx, dep_info):
  """Build arguments for Haskell package build.

  Args:
    ctx: Rule context.

  Returns:
    struct with the following fields:
      interfaces_dir: directory containing interface files
      interface_files: list of interface files
      object_files: list of static object files
      object_dyn_files: list of dynamic object files
      haddock_args: list of string arguments suitable for Haddock
      modules: set of module names
      source_files: set of Haskell module files
      import_dirs: import directories that should make all modules visible (for GHCi)
  """
  c = _compilation_defaults(ctx, dep_info)

  # This is absolutely required otherwise GHC doesn't know what package it's
  # creating `Name`s for to put them in Haddock interface files which then
  # results in Haddock not being able to find names for linking in
  # environment after reading its interface file later.
  pkg_id = get_pkg_id(ctx)
  unit_id_args = ["-this-unit-id", pkg_id,
                  "-optP-DCURRENT_PACKAGE_KEY=\"{}\"".format(pkg_id)]

  c.args.add(unit_id_args)
  c.haddock_args.add(unit_id_args, before_each="--optghc")

  ctx.actions.run(
    inputs = c.inputs,
    outputs = c.outputs,
    progress_message = "Compiling {0}".format(ctx.attr.name),
    env = c.env,
    executable = tools(ctx).ghc,
    arguments = [c.args],
  )

  return struct(
    interfaces_dir = c.interfaces_dir,
    interface_files = c.interface_files,
    object_files = c.object_files,
    object_dyn_files = c.object_dyn_files,
    haddock_args = c.haddock_args,
    modules = c.modules,
    source_files = c.source_files,
    header_files = c.header_files,
    import_dirs = c.import_dirs,
  )
