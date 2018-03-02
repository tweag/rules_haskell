"""A Haskell toolchain."""

load(":path_utils.bzl",
     "declare_compiled",
     "target_unique_name",
     "module_name",
     "import_hierarchy_root",
)

load(":set.bzl", "set")

load(":tools.bzl",
     "get_compiler",
     "get_compiler_version",
     "get_ghc_pkg",
     "get_build_tools",
     "get_build_tools_path",
)

load(":hsc2hs.bzl",
     "hsc_to_hs",
)

load(":cc.bzl", "cc_headers")

load(":java_interop.bzl",
     "JavaInteropInfo",
     "java_interop_info",
)

load(":providers.bzl",
     "HaskellPackageInfo",
     "CcSkylarkApiProviderHacked",
)

load("@bazel_skylib//:lib.bzl", "paths", "dicts")

load(":mode.bzl",
     "is_profiling_enabled",
)

load(":utils.bzl",
     "produce_paths_module",
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
    "interface_files": "Interface files.",
    "env": "Default env vars."
  },
)

def _hs_srcs(ctx):
  """Return sources that correspond to a Haskell module."""
  # TODO This may be not entirely correct because it does not take into
  # account sources produced by the hsc2hs tool. This probably leads to
  # undeclared object files from those inputs.
  return [f for f in ctx.files.srcs if f.extension in ["hs", "hsc", "lhs"]]

def _get_lib_name(lib):
  """Return name of library by dropping extension and \"lib\" prefix.

  Args:
    lib: The library File.

  Returns:
    String: name of library.
  """

  base = lib.basename[3:] if lib.basename[:3] == "lib" else lib.basename
  n = base.find(".so.")
  end = paths.replace_extension(base, "") if n == -1 else base[:n]
  return end

def _get_external_libs_path(libs):
  """Return a String value for using as LD_LIBRARY_PATH or similar.

  Args:
    libs: Set of File: the libs that should be available.

  Returns:
    String: paths to the given libs separated by \":\".
  """
  return ":".join(set.to_list(set.map(libs, _get_external_lib_path)))

def _get_external_lib_path(lib):
  return paths.dirname(lib.path)

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
  ctx.actions.run_shell(
    inputs = [solib],
    outputs = [qualsolib],
    command = "ln -s $(realpath {0}) {1}".format(solib.path, qualsolib.path),
  )
  return qualsolib

def _is_shared_library(f):
  """Check if the given File is a shared library.

  Args:
    f: The File to check.

  Returns:
    Bool: True if the given file `f` is a shared library, False otherwise.
  """
  return f.extension == "so" or f.basename.find(".so.") != -1

def _add_external_libraries(args, libs):
  """Add options to `args` that allow us to link to `libs`.

  Args:
    args: Args object.
    libs: set of external shared libraries.
  """
  seen_libs = set.empty()
  for lib in set.to_list(libs):
    lib_name = _get_lib_name(lib)
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

def compile_haskell_bin(ctx):
  """Compile a Haskell target into object files suitable for linking.

  Args:
    ctx: Rule context.

  Returns:
    list of File: Compiled object files.
  """
  c = _compilation_defaults(ctx)
  c.args.add(["-main-is", ctx.attr.main_function])

  ctx.actions.run(
    inputs = c.inputs,
    outputs = c.outputs,
    progress_message = "Building {0}".format(ctx.attr.name),
    env = c.env,
    executable = get_compiler(ctx),
    arguments = [c.args]
  )

  return c.object_files

def link_haskell_bin(ctx, object_files):
  """Link Haskell binary.

  Args:
    ctx: Rule context.
    object_files: Object files to include during linking.

  Returns:
    File: Built Haskell executable.
  """
  # Create empty archive so that GHC has some input files to work on during linking
  #
  # https://github.com/facebook/buck/blob/126d576d5c07ce382e447533b57794ae1a358cc2/src/com/facebook/buck/haskell/HaskellDescriptionUtils.java#L295
  dummy_input = ctx.actions.declare_file("BazelDummy.hs")
  dummy_object = ctx.actions.declare_file(paths.replace_extension("BazelDummy", ".o"))

  ctx.actions.write(output=dummy_input, content="\n".join([
    "{-# LANGUAGE NoImplicitPrelude #-}",
    "module BazelDummy () where"
  ]))

  dummy_static_lib = ctx.actions.declare_file("libempty.a")
  dummy_args = ctx.actions.args()
  dummy_args.add(["-no-link", dummy_input])
  ctx.actions.run(
    inputs = [dummy_input],
    outputs = [dummy_object],
    executable = get_compiler(ctx),
    arguments = [dummy_args]
  )
  ar_args = ctx.actions.args()
  ar_args.add(["qc", dummy_static_lib, dummy_object])

  ctx.actions.run(
    inputs = [dummy_object],
    outputs = [dummy_static_lib],
    executable = ctx.host_fragments.cpp.ar_executable,
    arguments = [ar_args]
  )

  args = ctx.actions.args()

  _add_mode_options(ctx, args)

  args.add(ctx.attr.compiler_flags)
  args.add(["-o", ctx.outputs.executable.path, dummy_static_lib.path])

  for o in object_files:
    args.add(["-optl", o.path])

  dep_info = gather_dependency_information(ctx)
  # De-duplicate optl calls while preserving ordering: we want last
  # invocation of an object to remain last. That is `-optl foo -optl
  # bar -optl foo` becomes `-optl bar -optl foo`. Do this by counting
  # number of occurrences. That way we only build dict and add to args
  # directly rather than doing multiple reversals with temporary
  # lists.
  link_paths = {}

  for lib in dep_info.static_libraries:
    link_paths[lib] = link_paths.get(lib, 0) + 1

  for lib in dep_info.static_libraries:
    occ = link_paths.get(lib, 0)
    # This is the last occurrence of the lib, insert it.
    if occ == 1:
      args.add(["-optl", lib.path])
    link_paths[lib] = occ - 1

  # We have to remember to specify all (transitive) wired-in
  # dependencies or we can't find objects for linking.
  for p in set.to_list(dep_info.prebuilt_dependencies):
    args.add(["-package", p])

  _add_external_libraries(args, dep_info.external_libraries)

  so_symlink_prefix = paths.relativize(
    paths.dirname(ctx.outputs.executable.path),
    ctx.bin_dir.path,
  )

  # The resulting test executable should be able to find all external
  # libraries when it is run by Bazel. That is achieved by setting RPATH to
  # a relative path which when joined with working directory points to
  # symlinks which in turn point to shared libraries. This is quite similar
  # to the approach taken by cc_binary, cc_test, etc.:
  #
  # https://github.com/bazelbuild/bazel/blob/f98a7a2fedb3e714cef1038dcb85f83731150246/src/main/java/com/google/devtools/build/lib/rules/cpp/CppActionConfigs.java#L587-L605
  args.add(["-optl-Wl,-rpath," + so_symlink_prefix])

  ctx.actions.run(
    inputs = depset(transitive = [
      depset(dep_info.static_libraries),
      depset(object_files),
      depset([dummy_static_lib]),
      set.to_depset(dep_info.external_libraries),
      set.to_depset(get_build_tools(ctx)),
    ]),
    outputs = [ctx.outputs.executable],
    progress_message = "Linking {0}".format(ctx.outputs.executable.basename),
    env = {
      "PATH": get_build_tools_path(ctx)
    },
    executable = get_compiler(ctx),
    arguments = [args]
  )

  # New we have to figure out symlinks to shared libraries to create for
  # running tests.
  so_symlinks = {}

  for lib in set.to_list(dep_info.external_libraries):
    so_symlinks[paths.join(so_symlink_prefix, paths.basename(lib.path))] = lib

  return [DefaultInfo(
    executable = ctx.outputs.executable,
    files = depset([ctx.outputs.executable]),
    runfiles = ctx.runfiles(symlinks=so_symlinks),
  )]

def compile_haskell_lib(ctx):
  """Build arguments for Haskell package build.

  Args:
    ctx: Rule context.

  Returns:
    (File, list of File, list of File, list of File):
      Returns in following order:

        * Directory containing interface files
        * Interface files
        * Object files
        * Dynamic object files
        * Haddock args
  """
  c = _compilation_defaults(ctx)
  c.args.add([
    "-package-name", get_pkg_id(ctx),
    "-static", "-dynamic-too"
  ])

  # This is absolutely required otherwise GHC doesn't know what package it's
  # creating `Name`s for to put them in Haddock interface files which then
  # results in Haddock not being able to find names for linking in
  # environment after reading its interface file later.
  unit_id_args = ["-this-unit-id", get_pkg_id(ctx)]

  c.args.add(unit_id_args)
  c.haddock_args.add(unit_id_args, before_each="--optghc")

  object_dyn_files = [
    declare_compiled(ctx, s, ".dyn_o", directory=c.objects_dir)
    for s in _hs_srcs(ctx)
  ]

  ctx.actions.run(
    inputs = c.inputs,
    outputs = c.outputs + object_dyn_files,
    progress_message = "Compiling {0}".format(ctx.attr.name),
    env = c.env,
    executable = get_compiler(ctx),
    arguments = [c.args],
  )

  return c.interfaces_dir, c.interface_files, c.object_files, object_dyn_files, c.haddock_args

def create_static_library(ctx, object_files):
  """Create a static library for the package using given object files.

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
    inputs = object_files,
    outputs = [static_library],
    executable = ctx.host_fragments.cpp.ar_executable,
    arguments = [args],
  )
  return static_library

def create_dynamic_library(ctx, object_files):
  """Create a dynamic library for the package using given object files.

  Args:
    ctx: Rule context.
    object_files: Object files to use for linking.

  Returns:
    File: Produced dynamic library.
  """

  version = get_compiler_version(ctx)
  dynamic_library = ctx.actions.declare_file(
    "lib{0}-ghc{1}.so".format(_get_library_name(ctx), version)
  )

  args = ctx.actions.args()

  _add_mode_options(ctx, args)

  args.add(["-shared", "-dynamic", "-o", dynamic_library.path])

  dep_info = gather_dependency_information(ctx)

  for n in depset(transitive = [dep_info.names, depset(ctx.attr.prebuilt_dependencies)]).to_list():
    args.add(["-package", n])

  for c in dep_info.caches.to_list():
    args.add(["-package-db", c.dirname])

  _add_external_libraries(args, dep_info.external_libraries)

  args.add([ f.path for f in object_files ])

  ctx.actions.run(
    inputs = depset(transitive = [
      depset(object_files),
      dep_info.caches,
      set.to_depset(dep_info.dynamic_libraries),
      set.to_depset(dep_info.external_libraries),
      set.to_depset(get_build_tools(ctx))]),
    outputs = [dynamic_library],
    progress_message = "Linking dynamic library {0}".format(dynamic_library.basename),
    env = {
      "PATH": get_build_tools_path(ctx),
    },
    executable = get_compiler(ctx),
    arguments = [args]
  )

  return dynamic_library

def create_ghc_package(ctx, interfaces_dir, static_library, dynamic_library):
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

  # Infer collection of public modules in the library.

  hidden_modules = set.from_list(ctx.attr.hidden_modules)
  public_modules = []

  for f in _hs_srcs(ctx):
    mname = module_name(ctx, f)
    if not set.is_member(hidden_modules, mname):
      public_modules.append(mname)

  # Create a file from which ghc-pkg will create the actual package from.
  registration_file = ctx.actions.declare_file(target_unique_name(ctx, "registration-file"))
  registration_file_entries = {
    "name": ctx.attr.name,
    "version": ctx.attr.version,
    "id": get_pkg_id(ctx),
    "key": get_pkg_id(ctx),
    "exposed": "True",
    "exposed-modules": " ".join(public_modules),
    "hidden-modules": " ".join(ctx.attr.hidden_modules),
    "import-dirs": paths.join("${pkgroot}", interfaces_dir.basename),
    "library-dirs": "${pkgroot}",
    "dynamic-library-dirs": "${pkgroot}",
    "hs-libraries": _get_library_name(ctx),
    "depends":
      ", ".join([ d[HaskellPackageInfo].name for d in ctx.attr.deps if HaskellPackageInfo in d])
  }
  ctx.actions.write(
    output=registration_file,
    content="\n".join(['{0}: {1}'.format(k, v)
                       for k, v in registration_file_entries.items()])
  )

  dep_info = gather_dependency_information(ctx)
  # Make the call to ghc-pkg and use the registration file
  package_path = ":".join([c.dirname for c in dep_info.confs.to_list()])
  ctx.actions.run(
    inputs = depset(transitive = [
      dep_info.confs,
      dep_info.caches,
      depset([static_library, interfaces_dir, registration_file, dynamic_library]),
      set.to_depset(get_build_tools(ctx))
    ]),
    outputs = [pkg_db_dir, conf_file, cache_file],
    env = {
      "GHC_PACKAGE_PATH": package_path,
      "PATH": get_build_tools_path(ctx),
    },
    executable = get_ghc_pkg(ctx),
    arguments = [
      "register", "--package-db={0}".format(pkg_db_dir.path),
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
    _DefaultCompile: Populated default compilation settings.
  """
  args = ctx.actions.args()
  haddock_args = ctx.actions.args()

  # Preprocess any sources
  sources = hsc_to_hs(ctx)

  # Declare file directories
  objects_dir_raw    = target_unique_name(ctx, "objects")
  objects_dir        = ctx.actions.declare_directory(objects_dir_raw)
  interfaces_dir_raw = target_unique_name(ctx, "interfaces")
  interfaces_dir     = ctx.actions.declare_directory(interfaces_dir_raw)

  # Compilation mode and explicit user flags
  if ctx.var["COMPILATION_MODE"] == "opt":
    args.add("-O2")

  args.add(ctx.attr.compiler_flags)
  haddock_args.add(ctx.attr.compiler_flags, before_each="--optghc")

  # Common flags
  args.add([
    "-c",
    "--make",
    "-fPIC",
    "-hide-all-packages",
  ])
  haddock_args.add(["-hide-all-packages"], before_each="--optghc")

  args.add([
    "-odir", objects_dir,
    "-hidir", interfaces_dir,
  ])

  _add_mode_options(ctx, args)

  ih_root_arg = ["-i{0}".format(import_hierarchy_root(ctx))]
  args.add(ih_root_arg)
  haddock_args.add(ih_root_arg, before_each="--optghc")

  dep_info = gather_dependency_information(ctx)

  # Expose all prebuilt dependencies
  for prebuilt_dep in ctx.attr.prebuilt_dependencies:
    items = ["-package", prebuilt_dep]
    args.add(items)
    haddock_args.add(items, before_each="--optghc")

  # Expose all bazel dependencies
  for package in dep_info.names.to_list():
    items = ["-package", package]
    args.add(items)
    if package != get_pkg_id(ctx):
      haddock_args.add(items, before_each="--optghc")

  # Only include package DBs for deps, prebuilt deps should be found
  # auto-magically by GHC.
  for c in dep_info.caches.to_list():
    items = ["-package-db", c.dirname]
    args.add(items)
    haddock_args.add(items, before_each="--optghc")

  # We want object and dynamic objects from all inputs.
  object_files = []

  # We need to keep interface files we produce so we can import
  # modules cross-package.
  interface_files = []

  textual_headers = []

  # Output object files are named after modules, not after input file names.
  # The difference is only visible in the case of Main module because it may
  # be placed in a file with a name different from "Main.hs". In that case
  # still Main.o will be produced.

  for s in _hs_srcs(ctx):

    if s.extension == "h":
      textual_headers.append(s)
    elif not hasattr(ctx.file, "main_file") or (s != ctx.file.main_file):
      object_files.append(
        declare_compiled(ctx, s, ".o", directory=objects_dir)
      )
      interface_files.append(
        declare_compiled(ctx, s, ".hi", directory=interfaces_dir)
      )
    else:
      object_files.append(
        ctx.actions.declare_file(paths.join(objects_dir_raw, "Main.o"))
      )
      interface_files.append(
        ctx.actions.declare_file(paths.join(interfaces_dir_raw, "Main.hi"))
      )

  hdrs, include_args = cc_headers(ctx)
  args.add(include_args)
  haddock_args.add(include_args, before_each="--optghc")

  # Lastly add all the processed sources.
  for f in sources:
    if f.extension not in ["hs-boot", "lhs-boot"]:
      args.add(f)
      haddock_args.add(f)

  # Generate and provide Paths_* module.

  paths_filename, paths_content = produce_paths_module(ctx.attr.name, ctx.attr.version)
  paths_file = ctx.actions.declare_file(paths_filename)

  ctx.actions.write(paths_file, paths_content)
  args.add(paths_file)
  haddock_args.add(paths_file)
  object_files.append(
    ctx.actions.declare_file(
      paths.join(
        objects_dir_raw,
        paths.replace_extension(paths_filename, ".o"),
      )
    )
  )

  # Add any interop info for other languages.
  java = java_interop_info(ctx)

  return _DefaultCompileInfo(
    args = args,
    haddock_args = haddock_args,
    inputs = depset(transitive = [
      depset(sources),
      depset(hdrs),
      dep_info.confs,
      dep_info.caches,
      set.to_depset(dep_info.interface_files),
      set.to_depset(dep_info.dynamic_libraries),
      set.to_depset(get_build_tools(ctx)),
      set.to_depset(dep_info.external_libraries),
      java.inputs,
      depset(textual_headers),
      depset([paths_file]),
    ]),
    outputs = [objects_dir, interfaces_dir] + object_files + interface_files,
    objects_dir = objects_dir,
    interfaces_dir = interfaces_dir,
    object_files = object_files,
    interface_files = interface_files,
    env = dicts.add({
      "PATH": get_build_tools_path(ctx),
      "LD_LIBRARY_PATH": _get_external_libs_path(dep_info.external_libraries),
      },
      java.env,
    ),
  )

def get_pkg_id(ctx):
  """Get package identifier. This is name-version.

  Args:
    ctx: Rule context

  Returns:
    string: GHC package ID to use.
  """
  return "{0}-{1}".format(ctx.attr.name, ctx.attr.version)

def _get_library_name(ctx):
  """Get core library name for this package. This is "HS" followed by package ID.

  See https://ghc.haskell.org/trac/ghc/ticket/9625 .

  Args:
    ctx: Rule context.

  Returns:
    string: Library name suitable for GHC package entry.
  """
  return "HS{0}".format(get_pkg_id(ctx))

def gather_dependency_information(ctx):
  """Collapse dependencies into a single HaskellPackageInfo.

  "name", "prebuilt_dependencies" and "external_libraries" fields are
  fully pre-populated.

  Args:
    ctx: Rule context.

  Returns:
    HaskellPackageInfo: Unified information about all dependencies needed during build.
  """
  hpi = HaskellPackageInfo(
    name = get_pkg_id(ctx),
    names = depset(),
    confs = depset(),
    caches = depset(),
    static_libraries = [],
    dynamic_libraries = set.empty(),
    interface_files = set.empty(),
    prebuilt_dependencies = set.from_list(ctx.attr.prebuilt_dependencies),
    external_libraries = set.empty(),
    haddock_ghc_args = None,
  )

  for dep in ctx.attr.deps:
    if HaskellPackageInfo in dep:
      pkg = dep[HaskellPackageInfo]
      hpi = HaskellPackageInfo(
        name = hpi.name,
        names = hpi.names + [pkg.name],
        confs = hpi.confs + pkg.confs,
        caches = hpi.caches + pkg.caches,
        static_libraries = hpi.static_libraries + pkg.static_libraries,
        dynamic_libraries = set.mutable_union(hpi.dynamic_libraries, pkg.dynamic_libraries),
        interface_files = set.mutable_union(hpi.interface_files, pkg.interface_files),
        prebuilt_dependencies = set.mutable_union(hpi.prebuilt_dependencies, pkg.prebuilt_dependencies),
        external_libraries = set.mutable_union(hpi.external_libraries, pkg.external_libraries),
        haddock_ghc_args = None,
      )
    else:
      # If not a Haskell dependency, pass it through as-is to the
      # linking phase.
      hpi = HaskellPackageInfo(
        name = hpi.name,
        names = hpi.names,
        confs = hpi.confs,
        caches = hpi.caches,
        static_libraries = hpi.static_libraries,
        dynamic_libraries = hpi.dynamic_libraries,
        interface_files = hpi.interface_files,
        prebuilt_dependencies = hpi.prebuilt_dependencies,
        external_libraries = set.mutable_union(
          hpi.external_libraries,
          set.from_list([
            # If the provider is CcSkylarkApiProviderHacked, then the .so
            # files come from haskell_cc_import.
            _mangle_solib(ctx, dep.label, f, CcSkylarkApiProviderHacked in dep)
            for f in dep.files.to_list() if _is_shared_library(f)
          ]),
        ),
        haddock_ghc_args = None,
      )

  return hpi
