"""A Haskell toolchain."""

load(":path_utils.bzl",
     "declare_compiled",
     "mk_name",
     "path_to_module",
)

load(":tools.bzl",
     "get_ar",
     "get_compiler",
     "get_ghc_pkg",
     "get_build_tools",
     "get_build_tools_path",
)

load("@bazel_skylib//:lib.bzl", "paths")

HaskellPackageInfo = provider(
  doc = "Package information exposed by Haskell libraries.",
  fields = {
    "name": "Package name, usually of the form name-version.",
    "names": "All package names of transitive dependencies. Includes own name.",
    "confs": "Package conf files.",
    "caches": "Package cache files.",
    "static_libraries": "Compiled library archives.",
    "dynamic_libraries": "Dynamic libraries.",
    "interface_files": "Interface files belonging to the packages.",
    "static_library_dirs": "Library file directories",
    "dynamic_library_dirs": "Dynamic library file directories",
    "prebuilt_dependencies": "Transitive collection of all wired-in Haskell dependencies.",
    "external_libraries": "Non-Haskell libraries needed for linking. List of shared libs.",
  }
)

def compile_haskell_bin(ctx):
  """Build arguments for Haskell binary object building.

  Args:
    ctx: Rule context.
  """
  object_dir = ctx.actions.declare_directory(mk_name(ctx, "objects"))
  object_files = [declare_compiled(ctx, s, ".o", directory=object_dir)
                  for s in ctx.files.srcs]
  args = ctx.actions.args()
  args.add(ctx.attr.compiler_flags)
  args.add("-no-link")
  args.add(ctx.files.srcs)
  args.add(["-odir", object_dir])
  args.add(["-main-is", ctx.attr.main])

  dep_info = gather_dependency_information(ctx)
  for n in dep_info.names.to_list():
    args.add(["-package", n])

  for db in depset([c.dirname for c in dep_info.confs.to_list()]).to_list():
    args.add(["-package-db", db])

  ctx.actions.run(
    inputs = dep_info.interface_files + dep_info.confs + dep_info.caches +
             dep_info.dynamic_libraries + ctx.files.srcs + get_build_tools(ctx),
    outputs = object_files + [object_dir],
    progress_message = "Building {0}".format(ctx.attr.name),
    env = {
      "PATH": get_build_tools_path(ctx)
    },
    executable = get_compiler(ctx),
    arguments = [args]
  )

  return object_files

def link_haskell_bin(ctx, object_files):
  """Link Haskell binary.

  Args:
    ctx: Rule context.
    object_files: Object files to include during linking.
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
    executable = get_ar(ctx),
    arguments = [ar_args]
  )

  # XXX Use list instead of Args, because no Args -> string conversion
  # function yet, and need to construct full shell command below as
  # a string.
  link_args = []
  link_args.extend(["-o", ctx.outputs.executable.path, dummy_static_lib.path])

  for o in object_files:
    link_args.extend(["-optl", o.path])

  dep_info = gather_dependency_information(ctx)
  for lib in dep_info.static_libraries.to_list():
    link_args.extend(["-optl", lib.path])

  # We have to remember to specify all (transitive) wired-in
  # dependencies or we can't find objects for linking.
  for p in dep_info.prebuilt_dependencies.to_list():
    link_args.extend(["-package", p])

  # Resolve symlinks to system libraries to their absolute location.
  # Otherwise we'd end up with meaningless relative rpath.
  rpaths = ["$(realpath {0})".format(lib.path) for lib in dep_info.external_libraries.to_list()]
  ctx.actions.run_shell(
    inputs = dep_info.static_libraries + object_files + [dummy_static_lib] +
             dep_info.external_libraries + get_build_tools(ctx) + dep_info.external_libraries,
    outputs = [ctx.outputs.executable],
    progress_message = "Linking {0}".format(ctx.outputs.executable.basename),
    command = " ".join([get_compiler(ctx).path] + rpaths + link_args),
  )

def compile_haskell_lib(ctx, generated_hs_sources):
  """Build arguments for Haskell package build.

  Args:
    ctx: Rule context.
    generated_hs_sources: Generated Haskell files to include in the build.
  """

  objects_dir = ctx.actions.declare_directory(mk_name(ctx, "objects"))
  interfaces_dir = ctx.actions.declare_directory(mk_name(ctx, "interfaces"))
  args = ctx.actions.args()
  args.add(ctx.attr.compiler_flags)
  args.add([
    "-no-link",
    "-hide-all-packages",
    "-package-name", "{0}-{1}".format(ctx.attr.name, ctx.attr.version),
    "-static", "-dynamic-too",
    "-odir", objects_dir.path, "-hidir", interfaces_dir.path
  ])

  dep_info = gather_dependency_information(ctx)
  for n in depset(transitive = [dep_info.names, depset(ctx.attr.prebuilt_dependencies)]).to_list():
    args.add(["-package", n])

  # Only include package DBs for deps, prebuilt deps should be found
  # auto-magically by GHC.
  for c in dep_info.caches.to_list():
    args.add(["-package-db", c.dirname])

  args.add(generated_hs_sources)
  args.add(ctx.files.srcs)

  external_files = depset([f for dep in ctx.attr.external_deps
                             for f in dep.files])

  # We want object and dynamic objects from all inputs.
  object_files = [declare_compiled(ctx, s, ".o", directory=objects_dir)
                  for s in get_input_files(ctx)]
  object_dyn_files = [declare_compiled(ctx, s, ".dyn_o", directory=objects_dir)
                      for s in get_input_files(ctx)]

  # We need to keep interface files we produce so we can import
  # modules cross-package.
  interface_files = [declare_compiled(ctx, s, ".hi", directory=interfaces_dir)
                     for s in get_input_files(ctx)]

  ctx.actions.run(
    inputs =
    ctx.files.srcs +
    generated_hs_sources +
    dep_info.caches.to_list() +
    external_files.to_list() +
    dep_info.interface_files.to_list() +
    dep_info.dynamic_libraries.to_list() +
    get_build_tools(ctx).to_list() +
    dep_info.external_libraries.to_list(),
    outputs = [interfaces_dir, objects_dir] + object_files + interface_files +
              object_dyn_files,
    progress_message = "Compiling {0}".format(ctx.attr.name),
    env = {
      "PATH": get_build_tools_path(ctx),

    },
    executable = get_compiler(ctx),
    arguments = [args],
  )

  return interfaces_dir, interface_files, object_files, object_dyn_files

def create_static_library(ctx, object_files):
  """Create a static library for the package using given object files.

  Args:
    ctx: Rule context.
    object_files: All object files to include in the library.
  """
  static_library_dir = ctx.actions.declare_directory(mk_name(ctx, "lib"))
  static_library = ctx.actions.declare_file(paths.join(static_library_dir.basename, "lib{0}.a".format(get_library_name(ctx))))

  args = ctx.actions.args()
  args.add(["qc", static_library])
  args.add(object_files)

  ctx.actions.run(
    inputs = object_files,
    outputs = [static_library, static_library_dir],
    executable = get_ar(ctx),
    arguments = [args],
  )
  return static_library_dir, static_library

def create_dynamic_library(ctx, object_files):
  """Create a dynamic library for the package using given object files.

  Args:
    ctx: Rule context.
    object_files: Object files to use for linking.
  """

  # Make shared library
  dynamic_library_dir = ctx.actions.declare_directory(mk_name(ctx, "dynlib"))
  dynamic_library = ctx.actions.declare_file(
    paths.join(dynamic_library_dir.basename,
               "lib{0}-ghc{1}.so".format(get_library_name(ctx), ctx.attr.ghc_version))
  )

  args = ["-shared", "-dynamic", "-o", dynamic_library.path]

  dep_info = gather_dependency_information(ctx)

  for n in depset(transitive = [dep_info.names, depset(ctx.attr.prebuilt_dependencies)]).to_list():
    args.extend(["-package", n])

  for c in dep_info.caches.to_list():
    args.extend(["-package-db", c.dirname])

  linker_flags = []
  for lib in dep_info.external_libraries.to_list():
    lib_name = paths.replace_extension(
      lib.basename[len("lib"):]
      if lib.basename[:len("lib")] == "lib"
      else lib.basename,
      "")
    linker_flags += [
      "-l{0}".format(lib_name),
      "-L$(dirname $(realpath {0}))".format(lib.path)
    ]
  args.extend(linker_flags)

  args.extend([ f.path for f in object_files ])

  ctx.actions.run_shell(
    inputs = depset(transitive = [depset(object_files),
                                  dep_info.caches,
                                  dep_info.dynamic_libraries,
                                  dep_info.external_libraries,
                                  get_build_tools(ctx)]),
    outputs = [dynamic_library_dir, dynamic_library],
    env = {
      "PATH": get_build_tools_path(ctx),
    },
    command = " ".join([get_compiler(ctx).path] + args)
  )

  return dynamic_library_dir, dynamic_library

def create_ghc_package(ctx, interfaces_dir, static_library, static_library_dir, dynamic_library_dir):
  """Create GHC package using ghc-pkg.

  Args:
    ctx: Rule context.
    interface_files: Interface files of the package.
    interfaces_dir: Directory containing interface files.
    static_library: Static library of the package.
    static_library_dir: Directory containing static library.
    dynamic_library_dir: Directory containing dynamic library.
  """
  pkg_db_dir = ctx.actions.declare_directory(get_pkg_id(ctx))
  conf_file = ctx.actions.declare_file(paths.join(pkg_db_dir.basename, "{0}.conf".format(get_pkg_id(ctx))))
  cache_file = ctx.actions.declare_file("package.cache", sibling=conf_file)

  # Create a file from which ghc-pkg will create the actual package from.
  registration_file = ctx.actions.declare_file(mk_name(ctx, "registration-file"))
  registration_file_entries = {
    "name": ctx.attr.name,
    "version": ctx.attr.version,
    "id": get_pkg_id(ctx),
    "key": get_pkg_id(ctx),
    "exposed": "True",
    "exposed-modules":
      " ".join([path_to_module(ctx, f) for f in get_input_files(ctx)]),
    "import-dirs": paths.join("${pkgroot}", interfaces_dir.basename),
    "library-dirs": paths.join("${pkgroot}", static_library_dir.basename),
    "dynamic-library-dirs":
      paths.join("${pkgroot}", dynamic_library_dir.basename),
    "hs-libraries": get_library_name(ctx),
    "depends":
      ", ".join([ d[HaskellPackageInfo].name for d in ctx.attr.deps if HaskellPackageInfo in d])
  }
  ctx.actions.write(
    output=registration_file,
    content="\n".join(['{0}: {1}'.format(k, v)
                       for k, v in registration_file_entries.items()])
  )

  pkg_confs = depset([
    c for dep in ctx.attr.deps
      if HaskellPackageInfo in dep
      for c in dep[HaskellPackageInfo].confs.to_list()
  ])

  # Make the call to ghc-pkg and use the registration file
  package_path = ":".join([c.dirname for c in pkg_confs.to_list()])
  ctx.actions.run(
    inputs = pkg_confs.to_list() + [static_library, interfaces_dir, registration_file] +
             get_build_tools(ctx).to_list(),
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

def get_pkg_id(ctx):
  """Get package identifier. This is name-version.

  Args:
    ctx: Rule context
  """
  return "{0}-{1}".format(ctx.attr.name, ctx.attr.version)

def get_library_name(ctx):
  """Get core library name for this package. This is "HS" followed by package ID.

  See https://ghc.haskell.org/trac/ghc/ticket/9625 .

  Args:
    ctx: Rule context.
  """
  return "HS{0}".format(get_pkg_id(ctx))

def get_input_files(ctx):
  """Get all files we expect to project object files from.

  Args:
    ctx: Rule context.
  """
  return ctx.files.srcs + ctx.files.hscs

def gather_dependency_information(ctx):
  """Collapse dependencies into a single HaskellPackageInfo.

  "name", "prebuilt_dependencies" and "external_libraries" fields are
  fully pre-populated.

  Args:
    ctx: Rule context.

  """
  hpi = HaskellPackageInfo(
    name = get_pkg_id(ctx),
    names = depset(),
    confs = depset(),
    caches = depset(),
    static_libraries = depset(),
    dynamic_libraries = depset(),
    interface_files = depset(),
    static_library_dirs = depset(),
    dynamic_library_dirs = depset(),
    prebuilt_dependencies = depset(ctx.attr.prebuilt_dependencies),
    external_libraries = depset(),
  )

  for dep in ctx.attr.deps:
    if HaskellPackageInfo in dep:
      pkg = dep[HaskellPackageInfo]
      new_external_libraries = hpi.external_libraries
      new_external_libraries = new_external_libraries.union(pkg.external_libraries)
      hpi = HaskellPackageInfo(
        name = hpi.name,
        names = hpi.names + pkg.names,
        confs = hpi.confs + pkg.confs,
        caches = hpi.caches + pkg.caches,
        static_libraries = hpi.static_libraries + pkg.static_libraries,
        dynamic_libraries = hpi.dynamic_libraries + pkg.dynamic_libraries,
        interface_files = hpi.interface_files + pkg.interface_files,
        static_library_dirs = hpi.static_library_dirs + pkg.static_library_dirs,
        dynamic_library_dirs = hpi.dynamic_library_dirs + pkg.dynamic_library_dirs,
        prebuilt_dependencies = hpi.prebuilt_dependencies + pkg.prebuilt_dependencies,
        external_libraries = new_external_libraries,
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
        static_library_dirs = hpi.static_library_dirs,
        dynamic_library_dirs = hpi.dynamic_library_dirs,
        prebuilt_dependencies = hpi.prebuilt_dependencies,
        external_libraries = hpi.external_libraries.union(depset(dep.files)),
      )
  return hpi
