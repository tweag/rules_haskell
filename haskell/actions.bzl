"""A Haskell toolchain."""

load(":path_utils.bzl",
     "declare_compiled",
     "target_unique_name",
     "module_name",
     "module_unique_name",
     "import_hierarchy_root",
     "get_external_libs_path",
)

load(":set.bzl", "set")

load(":tools.bzl",
     "get_build_tools_path",
     "get_ghc_version",
     "is_darwin",
     "so_extension",
     "tools",
)

load(":cc.bzl", "cc_headers")

load(":java_interop.bzl",
     "JavaInteropInfo",
     "java_interop_info",
)

load(":providers.bzl",
     "HaskellBuildInfo",
     "HaskellLibraryInfo",
     "HaskellBinaryInfo",
     "CcSkylarkApiProviderHacked",
)

load("@bazel_skylib//:lib.bzl", "paths", "dicts")

load(":mode.bzl",
     "is_profiling_enabled",
)

load(":utils.bzl",
     "get_lib_name",
)

_DefaultCompileInfo = provider(
  doc = "Default compilation files and configuration.",
  fields = {
    "args": "Default argument list.",
    "haddock_args": "Default Haddock argument list.",
    "inputs": "Default inputs.",
    "outputs": "Default outputs.",
    "objects_dir": "Object files directory.",
    "interfaces_dir": "Interface files directory.",
    "object_files": "Object files.",
    "object_dyn_files": "Dynamic object files.",
    "interface_files": "Interface files.",
    "modules": "Set of all module names.",
    "source_files": "Set of files that contain Haskell modules",
    "import_dirs": "Import hierarchy roots.",
    "env": "Default env vars."
  },
)

def _truly_relativize(target, relative_to):
  """Return a relative path to `target` from `relative_to`.

  Args:
    target: string, path to directory we want to get relative path to.
    relative_to: string, path to directory from which we are starting.

  Returns:
    string: relative path to `target`.
  """
  t_pieces = target.split('/')
  r_pieces = relative_to.split('/')
  common_part_len = 0

  for tp, rp in zip(t_pieces, r_pieces):
    if tp == rp:
      common_part_len += 1
    else:
      break

  result = [".."] * (len(r_pieces) - common_part_len)
  result += t_pieces[common_part_len:]

  return "/".join(result)

def _mangle_solib(ctx, label, solib, preserve_name):
  """Create a symlink to a dynamic library, with a longer name.

  The built-in cc_* rules don't link against a shared library
  directly. They link against a symlink whose name is guaranteed to be
  unique across the entire workspace. This disambiguates dynamic
  libraries with the same soname. This process is called "mangling".
  The built-in rules don't expose mangling functionality directly (see
  https://github.com/bazelbuild/bazel/issues/4581). But this function
  emulates the built-in dynamic library mangling.

  Args:
    ctx: Rule context.
    label: the label to use as a qualifier for the dynamic library name.
    solib: the dynamic library.
    preserve_name: Bool, whether given `solib` should be returned unchanged.

  Returns:
    File: the created symlink or the original solib.
  """

  if preserve_name:
    return solib

  components = [c for c in [label.workspace_root, label.package, label.name] if c]
  qualifier = '/'.join(components).replace('_', '_U').replace('/', '_S')
  qualsolib = ctx.actions.declare_file("lib" + qualifier + "_" + solib.basename)

  # NOTE We only have relative paths at our disposal here, so we must
  # specify the link target as a path that is relative to the link location.
  # This allows us to avoid the $(realpath ...) hack and makes the resulting
  # directory tree movable (at least in theory).
  relative_solib = _truly_relativize(solib.path, qualsolib.dirname)

  ctx.actions.run(
    inputs = [solib],
    outputs = [qualsolib],
    executable = tools(ctx).ln,
    arguments = ["-s", relative_solib, qualsolib.path],
  )
  return qualsolib

def _is_shared_library(f):
  """Check if the given File is a shared library.

  Args:
    f: The File to check.

  Returns:
    Bool: True if the given file `f` is a shared library, False otherwise.
  """
  return f.extension in ["so", "dylib"] or f.basename.find(".so.") != -1

def _add_external_libraries(args, libs):
  """Add options to `args` that allow us to link to `libs`.

  Args:
    args: Args object.
    libs: list of external shared libraries.
  """
  seen_libs = set.empty()
  for lib in libs:
    lib_name = get_lib_name(lib)
    if not set.is_member(seen_libs, lib_name):
      set.mutable_insert(seen_libs, lib_name)
      args.add([
        "-l{0}".format(lib_name),
        "-L{0}".format(paths.dirname(lib.path)),
      ])

def _add_mode_options(ctx, args):
  """Add mode options to the given args object.

  Args:
    ctx: Rule context.
    args: args object.

  Returns:
    None
  """
  if is_profiling_enabled(ctx):
    args.add("-prof")

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

  ctx.actions.run(
    inputs = [ghc_defs_dump_raw, tools(ctx).grep],
    outputs = [ghc_defs_dump],
    executable = ctx.file._ghc_defs_cleanup,
    arguments = [
      tools(ctx).grep.path,
      ghc_defs_dump_raw.path,
      ghc_defs_dump.path,
    ],
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
  hdrs, include_args = cc_headers(ctx)
  args.add(["--cflag=" + f for f in include_args])
  args.add("-I{0}".format(ghc_defs_dump.dirname))
  args.add("-i{0}".format(ghc_defs_dump.basename))

  ctx.actions.run(
    inputs = depset(transitive = [
      depset(hdrs),
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

def _backup_path(target):
  """Return a path from the directory this is in to the Bazel root.

  Args:
    target: File

  Returns:
    A path of the form "../../.."
  """
  n = len(target.dirname.split("/"))

  return "/".join([".."] * n)

def _infer_rpaths(target, solibs):
  """Return set of RPATH values to be added to target so it can find all
  solibs.

  Args:
    target: File, executable or library we're linking.
    solibs: A set of Files, shared objects that the target needs.

  Returns:
    Set of strings: rpaths to add to target.
  """
  r = set.empty()

  for solib in set.to_list(solibs):
    rpath = paths.normalize(
      paths.join(
        _backup_path(target),
        solib.dirname,
      )
    )
    set.mutable_insert(r, "$ORIGIN/" + rpath)

  return r

def compile_haskell_bin(ctx):
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
  c = _compilation_defaults(ctx)
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

def _create_dummy_archive(ctx):
  """Create empty archive so that GHC has some input files to work on during
  linking.

  See: https://github.com/facebook/buck/blob/126d576d5c07ce382e447533b57794ae1a358cc2/src/com/facebook/buck/haskell/HaskellDescriptionUtils.java#L295

  Args:
    ctx: Rule context.

  Returns:
    File, the created dummy archive.
  """

  dummy_raw = "BazelDummy.hs"
  dummy_input = ctx.actions.declare_file(dummy_raw)
  dummy_object = ctx.actions.declare_file(paths.replace_extension(dummy_raw, ".o"))

  ctx.actions.write(output=dummy_input, content="""
{-# LANGUAGE NoImplicitPrelude #-}
module BazelDummy () where
""")

  dummy_static_lib = ctx.actions.declare_file("libempty.a")
  ctx.actions.run(
    inputs = [dummy_input],
    outputs = [dummy_object],
    executable = tools(ctx).ghc,
    arguments = ["-c", dummy_input.path],
  )

  ar_args = ctx.actions.args()
  ar_args.add(["qc", dummy_static_lib, dummy_object])

  ctx.actions.run(
    inputs = [dummy_object] + tools(ctx).ar_runfiles,
    outputs = [dummy_static_lib],
    executable = tools(ctx).ar,
    arguments = [ar_args]
  )

  return dummy_static_lib

def _fix_linker_paths(ctx, inp, out, external_libraries):
  """Postprocess a macOS binary to make shared library references relative.

  On macOS, in order to simulate the linker "rpath" behavior and make the
  binary load shared libraries from relative paths, (or dynamic libraries
  load other libraries) we need to postprocess it with install_name_tool.
  (This is what the Bazel-provided `cc_wrapper.sh` does for cc rules.)
  For details: https://blogs.oracle.com/dipol/entry/dynamic_libraries_rpath_and_mac

  Args:
    ctx: Rule context.
    inp: An input file.
    out: An output file.
    external_libraries: A list of C library dependencies to make relative.
  """
  ctx.actions.run_shell(
      inputs=[inp],
      outputs=[out],
      progress_message =
          "Fixing install paths for {0}".format(out.basename),
      command = " &&\n    ".join(
          ["cp {} {}".format(inp.path, out.path),
           "chmod +w {}".format(out.path)]
          + ["/usr/bin/install_name_tool -change {} {} {}"
             .format(f.path,
                     paths.join("@loader_path", _backup_path(out), f.path),
                     out.path)
                     for f in external_libraries]))

def link_haskell_bin(ctx, object_files):
  """Link Haskell binary from static object files.

  Args:
    ctx: Rule context.
    object_files: Dynamic object files.

  Returns:
    File: produced executable
  """

  dummy_static_lib = _create_dummy_archive(ctx)

  dep_info = gather_dep_info(ctx)

  if not is_darwin(ctx):
    compile_output = ctx.outputs.executable
  else:
    compile_output = ctx.actions.declare_file(ctx.outputs.executable.basename + ".temp")
    _fix_linker_paths(ctx, compile_output, ctx.outputs.executable,
                      dep_info.external_libraries)

  args = ctx.actions.args()
  _add_mode_options(ctx, args)
  args.add(ctx.attr.compiler_flags)

  if is_darwin(ctx):
    args.add(["-optl-Wl,-headerpad_max_install_names"])
  else:
    # TODO: enable dynamic linking of Haskell dependencies for macOS.
    args.add(["-dynamic", "-pie"])

  args.add(["-o", compile_output.path, dummy_static_lib.path])

  # De-duplicate optl calls while preserving ordering: we want last
  # invocation of an object to remain last. That is `-optl foo -optl
  # bar -optl foo` becomes `-optl bar -optl foo`. Do this by counting
  # number of occurrences. That way we only build dict and add to args
  # directly rather than doing multiple reversals with temporary
  # lists.

  args.add([f.path for f in object_files])

  for package in set.to_list(dep_info.package_ids):
    args.add(["-package-id", package])

  for cache in set.to_list(dep_info.package_caches):
    args.add(["-package-db", cache.dirname])

  # We have to remember to specify all (transitive) wired-in
  # dependencies or we can't find objects for linking.
  for p in set.to_list(dep_info.prebuilt_dependencies):
    args.add(["-package", p])

  _add_external_libraries(args, dep_info.external_libraries.values())

  solibs = set.union(
    set.from_list(dep_info.external_libraries),
    dep_info.dynamic_libraries,
  )

  if is_darwin(ctx):
    # Suppress a warning that Clang prints due to GHC automatically passing
    # "-pie" or "-no-pie" to the C compiler.
    # This particular invocation of GHC is a little unusual; e.g., we're
    # passing an empty archive so that GHC has some input files to work on
    # during linking.
    args.add(["-optc-Wno-unused-command-line-argument",
              "-optl-Wno-unused-command-line-argument"])
  else:
    for rpath in set.to_list(_infer_rpaths(ctx.outputs.executable, solibs)):
      args.add(["-optl-Wl,-rpath," + rpath])

  ctx.actions.run(
    inputs = depset(transitive = [
      set.to_depset(dep_info.package_caches),
      set.to_depset(dep_info.dynamic_libraries),
      depset(dep_info.static_libraries),
      depset(object_files),
      depset([dummy_static_lib]),
      depset(dep_info.external_libraries.values()),
    ]),
    outputs = [compile_output],
    progress_message = "Linking {0}".format(ctx.attr.name),
    executable = tools(ctx).ghc,
    arguments = [args]
  )

  return ctx.outputs.executable

def compile_haskell_lib(ctx):
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
  c = _compilation_defaults(ctx)

  # This is absolutely required otherwise GHC doesn't know what package it's
  # creating `Name`s for to put them in Haddock interface files which then
  # results in Haddock not being able to find names for linking in
  # environment after reading its interface file later.
  unit_id_args = ["-this-unit-id", get_pkg_id(ctx)]

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
    import_dirs = c.import_dirs,
  )

def link_static_lib(ctx, object_files):
  """Link a static library for the package using given object files.

  Args:
    ctx: Rule context.
    object_files: All object files to include in the library.

  Returns:
    File: Produced static library.
  """
  static_library = ctx.actions.declare_file("lib{0}.a".format(_get_library_name(ctx)))

  args = ctx.actions.args()
  args.add(["qc", static_library])
  args.add(object_files)

  ctx.actions.run(
    inputs = object_files + tools(ctx).ar_runfiles,
    outputs = [static_library],
    progress_message = "Linking static library {0}".format(static_library.basename),
    executable = tools(ctx).ar,
    arguments = [args],
  )
  return static_library

def link_dynamic_lib(ctx, object_files):
  """Link a dynamic library for the package using given object files.

  Args:
    ctx: Rule context.
    object_files: Object files to use for linking.

  Returns:
    File: Produced dynamic library.
  """

  version = get_ghc_version(ctx)
  dynamic_library = ctx.actions.declare_file(
    "lib{0}-ghc{1}.{2}".format(_get_library_name(ctx), version, so_extension(ctx))
  )

  args = ctx.actions.args()

  _add_mode_options(ctx, args)

  args.add(["-shared", "-dynamic"])

  dep_info = gather_dep_info(ctx)

  for package in set.to_list(dep_info.package_ids):
    args.add(["-package-id", package])
  for package in set.to_list(set.from_list(ctx.attr.prebuilt_dependencies)):
    args.add(["-package", package])

  for cache in set.to_list(dep_info.package_caches):
    args.add(["-package-db", cache.dirname])

  _add_external_libraries(args, dep_info.external_libraries.values())

  args.add([ f.path for f in object_files ])

  solibs = set.union(
    set.from_list(dep_info.external_libraries),
    dep_info.dynamic_libraries,
  )

  if is_darwin(ctx):
    dynamic_library_tmp = ctx.actions.declare_file(dynamic_library.basename + ".temp")
    _fix_linker_paths(ctx, dynamic_library_tmp, dynamic_library,
                      dep_info.external_libraries)
    args.add(["-optl-Wl,-headerpad_max_install_names"])
  else:
    dynamic_library_tmp = dynamic_library
    for rpath in set.to_list(_infer_rpaths(dynamic_library, solibs)):
      args.add(["-optl-Wl,-rpath," + rpath])

  args.add(["-o", dynamic_library_tmp.path])

  ctx.actions.run(
    inputs = depset(transitive = [
      depset(object_files),
      set.to_depset(dep_info.package_caches),
      set.to_depset(dep_info.dynamic_libraries),
      depset(dep_info.external_libraries.values()),
    ]),
    outputs = [dynamic_library_tmp],
    progress_message = "Linking dynamic library {0}".format(dynamic_library.basename),
    executable = tools(ctx).ghc,
    arguments = [args]
  )

  return dynamic_library

def create_ghc_package(ctx, interfaces_dir, static_library, dynamic_library, exposed_modules, other_modules):
  """Create GHC package using ghc-pkg.

  Args:
    ctx: Rule context.
    interfaces_dir: Directory containing interface files.
    static_library: Static library of the package.
    dynamic_library: Dynamic library of the package.

  Returns:
    (File, File): GHC package conf file, GHC package cache file
  """
  pkg_db_dir = ctx.actions.declare_directory(get_pkg_id(ctx))
  conf_file = ctx.actions.declare_file(paths.join(pkg_db_dir.basename, "{0}.conf".format(get_pkg_id(ctx))))
  cache_file = ctx.actions.declare_file("package.cache", sibling=conf_file)
  dep_info = gather_dep_info(ctx)

  # Create a file from which ghc-pkg will create the actual package from.
  registration_file = ctx.actions.declare_file(target_unique_name(ctx, "registration-file"))
  registration_file_entries = {
    "name": get_pkg_name(ctx),
    "version": ctx.attr.version,
    "id": get_pkg_id(ctx),
    "key": get_pkg_id(ctx),
    "exposed": "True",
    "exposed-modules": " ".join(set.to_list(exposed_modules)),
    "hidden-modules": " ".join(set.to_list(other_modules)),
    "import-dirs": paths.join("${pkgroot}", interfaces_dir.basename),
    "library-dirs": "${pkgroot}",
    "dynamic-library-dirs": "${pkgroot}",
    "hs-libraries": _get_library_name(ctx),
    "depends":
      ", ".join([ d[HaskellLibraryInfo].package_id for d in
                  ctx.attr.deps if HaskellLibraryInfo in d])
  }
  ctx.actions.write(
    output=registration_file,
    content="\n".join(['{0}: {1}'.format(k, v)
                       for k, v in registration_file_entries.items()])
  )

  # Make the call to ghc-pkg and use the registration file
  package_path = ":".join([c.dirname for c in set.to_list(dep_info.package_confs)])
  ctx.actions.run(
    inputs = depset(transitive = [
      set.to_depset(dep_info.package_confs),
      set.to_depset(dep_info.package_caches),
      depset([static_library, interfaces_dir, registration_file, dynamic_library]),
    ]),
    outputs = [pkg_db_dir, conf_file, cache_file],
    env = {
      "GHC_PACKAGE_PATH": package_path,
    },
    executable = tools(ctx).ghc_pkg,
    arguments = [
      "register", "--package-db={0}".format(pkg_db_dir.path),
      "-v0",
      "--no-expand-pkgroot",
      registration_file.path
    ]
  )

  return conf_file, cache_file

def _compilation_defaults(ctx):
  """Declare default compilation targets and create default compiler arguments.

  Args:
    ctx: Rule context.

  Returns:
    _DefaultCompileInfo: Populated default compilation settings.
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

  _add_mode_options(ctx, args)

  dep_info = gather_dep_info(ctx)

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

  hdrs, include_args = cc_headers(ctx)
  preprocessor_args = ["-optP" + f for f in include_args]
  args.add(preprocessor_args)
  haddock_args.add(preprocessor_args, before_each="--optghc")

  for f in set.to_list(source_files):
    args.add(f)
    haddock_args.add(f)

  # Add any interop info for other languages.
  java = java_interop_info(ctx)

  return _DefaultCompileInfo(
    args = args,
    haddock_args = haddock_args,
    inputs = depset(transitive = [
      set.to_depset(source_files),
      depset(hdrs),
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
    import_dirs = import_dirs,
    env = dicts.add({
      "LD_LIBRARY_PATH": get_external_libs_path(set.from_list(dep_info.external_libraries.values())),
      },
      java.env,
    ),
  )

def _zencode(s):
  """Zero-escape a given string.

  Args:
    s: inputs string.

  Returns:
    string: zero-escaped string.
  """
  return s.replace("Z", "ZZ").replace("_", "ZU").replace("/", "ZS")

def get_pkg_name(ctx):
  """Get package name.

  The name is not required to be unique/injective; however it must be a valid
  GHC package name (i.e., no underscores).  This encoding does not aim to
  change as little as possible since it is used for display and also for the
  "PackageImports" extension.

  Args:
    ctx: Rule context

  Returns:
    string: GHC package name to use.
  """
  return ctx.attr.name.replace("_", "-")

def get_pkg_id(ctx):
  """Get package identifier of the form `name-version`.

  The identifier is required to be unique for each Haskell rule.
  It includes the Bazel package and the name of the this component.
  We can't use just the latter because then two components with
  the same names in different packages would clash.

  Args:
    ctx: Rule context

  Returns:
    string: GHC package ID to use.
  """
  return "{0}-{1}".format(
    _zencode(paths.join(
        ctx.label.workspace_root,
        ctx.label.package,
        ctx.attr.name)),
    ctx.attr.version)

def _get_library_name(ctx):
  """Get core library name for this package. This is "HS" followed by package ID.

  See https://ghc.haskell.org/trac/ghc/ticket/9625 .

  Args:
    ctx: Rule context.

  Returns:
    string: Library name suitable for GHC package entry.
  """
  return "HS{0}".format(get_pkg_id(ctx))

def gather_dep_info(ctx):
  """Collapse dependencies into a single `HaskellBuildInfo`.

  Note that the field `prebuilt_dependencies` also includes
  prebuilt_dependencies of current target.

  Args:
    ctx: Rule context.

  Returns:
    HaskellBuildInfo: Unified information about all dependencies.
  """

  acc = HaskellBuildInfo(
    package_ids = set.empty(),
    package_confs = set.empty(),
    package_caches = set.empty(),
    static_libraries = [],
    dynamic_libraries = set.empty(),
    interface_files = set.empty(),
    prebuilt_dependencies = set.from_list(ctx.attr.prebuilt_dependencies),
    external_libraries = {},
  )

  for dep in ctx.attr.deps:
    if HaskellBuildInfo in dep:
      binfo = dep[HaskellBuildInfo]
      package_ids = acc.package_ids
      if HaskellBinaryInfo in dep:
        fail("Target {0} cannot depend on binary".format(ctx.attr.name))
      if HaskellLibraryInfo in dep:
        set.mutable_insert(package_ids, dep[HaskellLibraryInfo].package_id)
      acc = HaskellBuildInfo(
        package_ids = package_ids,
        package_confs = set.mutable_union(acc.package_confs, binfo.package_confs),
        package_caches = set.mutable_union(acc.package_caches, binfo.package_caches),
        static_libraries = acc.static_libraries + binfo.static_libraries,
        dynamic_libraries = set.mutable_union(acc.dynamic_libraries, binfo.dynamic_libraries),
        interface_files = set.mutable_union(acc.interface_files, binfo.interface_files),
        prebuilt_dependencies = set.mutable_union(acc.prebuilt_dependencies, binfo.prebuilt_dependencies),
        external_libraries = dicts.add(acc.external_libraries, binfo.external_libraries)
      )
    else:
      # If not a Haskell dependency, pass it through as-is to the
      # linking phase.
      acc = HaskellBuildInfo(
        package_ids = acc.package_ids,
        package_confs = acc.package_confs,
        package_caches = acc.package_caches,
        static_libraries = acc.static_libraries,
        dynamic_libraries = acc.dynamic_libraries,
        interface_files = acc.interface_files,
        prebuilt_dependencies = acc.prebuilt_dependencies,
        external_libraries = dicts.add(
          acc.external_libraries,
          {f:
            # If the provider is CcSkylarkApiProviderHacked, then the .so
            # files come from haskell_cc_import.
            _mangle_solib(ctx, dep.label, f, CcSkylarkApiProviderHacked in dep)
            for f in dep.files.to_list() if _is_shared_library(f)}

          ),
      )

  return acc
