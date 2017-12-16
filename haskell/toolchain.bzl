load(":path_utils.bzl",
     "declare_compiled",
     "get_dyn_interface_suffix",
     "get_dyn_object_suffix",
     "get_interface_suffix",
     "get_object_suffix",
     "mk_name",
     "path_append",
     "path_to_module",
     "replace_ext",
)

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
    "external_libraries": "Non-Haskell libraries needed for linking. Dict of Name:LinkDir.",
  }
)

def compile_haskell_bin(ctx):
  """Build arguments for Haskell binary object building.

  Args:
    ctx: Rule context.
  """
  object_dir = ctx.actions.declare_directory(mk_name(ctx, "objects"))
  object_files = [declare_compiled(ctx, s, get_object_suffix(), directory=object_dir)
                  for s in ctx.files.srcs]
  args = ctx.actions.args()
  args.add(ctx.attr.compiler_flags)
  args.add("-no-link")
  args.add(ctx.files.srcs)
  args.add(["-odir", object_dir])
  args.add(["-main-is", ctx.attr.main])
  args.add(["-osuf", get_object_suffix(), "-hisuf", get_interface_suffix()])

  dep_info = gather_dependency_information(ctx)
  for n in dep_info.names:
    args.add(["-package", n])

  for db in depset([ c.dirname for c in dep_info.confs ]):
    args.add(["-package-db", db])

  ctx.actions.run(
    inputs = dep_info.interface_files + dep_info.confs + dep_info.caches +
             dep_info.dynamic_libraries + ctx.files.srcs,
    outputs = object_files + [object_dir],
    # TODO: use env for GHC_PACKAGE_PATH when use_default_shell_env is removed
    use_default_shell_env = True,
    progress_message = "Building {0}".format(ctx.attr.name),
    executable = "ghc",
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
  dummy_object = ctx.actions.declare_file(replace_ext("BazelDummy", get_object_suffix()))

  ctx.actions.write(output=dummy_input, content="\n".join([
    "{-# LANGUAGE NoImplicitPrelude #-}",
    "module BazelDummy () where"
  ]))

  dummy_static_lib = ctx.actions.declare_file("libempty.a")
  dummy_args = ctx.actions.args()
  dummy_args.add(["-no-link", dummy_input, "-osuf", get_object_suffix()])
  ctx.actions.run(
    inputs = [dummy_input],
    outputs = [dummy_object],
    use_default_shell_env = True,
    executable = "ghc",
    arguments = [dummy_args]
  )
  ar_args = ctx.actions.args()
  ar_args.add(["qc", dummy_static_lib, dummy_object])

  ctx.actions.run(
    inputs = [dummy_object],
    outputs = [dummy_static_lib],
    use_default_shell_env = True,
    executable = "ar",
    arguments = [ar_args]
  )

  link_args = ctx.actions.args()
  link_args.add(["-o", ctx.outputs.executable, dummy_static_lib])

  for o in object_files:
    link_args.add(["-optl", o])

  dep_info = gather_dependency_information(ctx)
  for l in dep_info.static_libraries:
    link_args.add(["-optl", l])

  external_libraries = {}
  for dep in ctx.attr.deps:
    external_libraries.update(
      dep[HaskellPackageInfo].external_libraries
    )

  for dep_name, dep_dirs in external_libraries.items():
    link_args.add("-l{0}".format(dep_name))
    for d in dep_dirs:
      link_args.add("-L{0}".format(d))

  # We have to remember to specify all (transitive) wired-in
  # dependencies or we can't find objects for linking.
  for p in dep_info.prebuilt_dependencies:
    link_args.add(["-package", p])

  ctx.actions.run(
    inputs = dep_info.static_libraries + object_files + [dummy_static_lib],
    outputs = [ctx.outputs.executable],
    use_default_shell_env = True,
    progress_message = "Linking {0}".format(ctx.outputs.executable.basename),
    executable = "ghc",
    arguments = [link_args],
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
    "-osuf", get_object_suffix(),
    "-dynosuf", get_dyn_object_suffix(),
    "-hisuf", get_interface_suffix(),
    "-dynhisuf", get_dyn_interface_suffix(),
    "-odir", objects_dir, "-hidir", interfaces_dir
  ])

  dep_info = gather_dependency_information(ctx)
  for n in dep_info.names + depset(ctx.attr.prebuilt_dependencies):
    args.add(["-package", n])

  # Only include package DBs for deps, prebuilt deps should be found
  # auto-magically by GHC.
  for c in dep_info.caches:
    args.add(["-package-db", c.dirname])

  args.add(generated_hs_sources)
  args.add(ctx.files.srcs)

  external_files = depset([f for dep in ctx.attr.external_deps
                             for f in dep.files])

  # We want object and dynamic objects from all inputs.
  object_files = [declare_compiled(ctx, s, get_object_suffix(), directory=objects_dir)
                  for s in get_input_files(ctx)]
  object_dyn_files = [declare_compiled(ctx, s, get_dyn_object_suffix(), directory=objects_dir)
                      for s in get_input_files(ctx)]

  # We need to keep interface files we produce so we can import
  # modules cross-package.
  interface_files = [declare_compiled(ctx, s, get_interface_suffix(), directory=interfaces_dir)
                     for s in get_input_files(ctx)]
  ctx.actions.run(
    inputs =
    ctx.files.srcs + generated_hs_sources +
    (dep_info.caches + external_files + dep_info.interface_files +
     dep_info.dynamic_libraries).to_list(),
    outputs = [interfaces_dir, objects_dir] + object_files + interface_files +
    object_dyn_files,
    use_default_shell_env = True,
    progress_message = "Compiling {0}".format(ctx.attr.name),
    executable = "ghc",
    arguments = [args]
  )

  return interfaces_dir, interface_files, object_files, object_dyn_files

def create_static_library(ctx, object_files):
  """Create a static library for the package using given object files.

  Args:
    ctx: Rule context.
    object_files: All object files to include in the library.
  """
  args = ctx.actions.args()
  static_library_dir = ctx.actions.declare_directory(mk_name(ctx, "lib"))
  static_library = ctx.actions.declare_file(path_append(static_library_dir.basename, "lib{0}.a".format(get_library_name(ctx))))


  args = ctx.actions.args()
  args.add(["qc", static_library])
  args.add(object_files)

  ctx.actions.run(
    inputs = object_files,
    outputs = [static_library, static_library_dir],
    use_default_shell_env = True,
    executable = "ar",
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
    path_append(dynamic_library_dir.basename,
                "lib{0}-ghc{1}.so".format(get_library_name(ctx), ctx.attr.ghcVersion))
  )

  args = ctx.actions.args()
  args.add(["-shared", "-dynamic", "-o", dynamic_library])

  dep_info = gather_dependency_information(ctx)

  # We do not add anything to inputs from these as these are supposed
  # to be absolute paths outside of bazel's control such as system
  # libraries.
  for dep_name, dep_dirs in dep_info.external_libraries.items():
    args.add("-l{0}".format(dep_name))
    for d in dep_dirs:
      args.add("-L{0}".format(d))

  pkg_caches = depset()
  pkg_names = depset()
  dep_dyn_lib_dirs = depset()
  for d in ctx.attr.deps:
    pkg_caches += d[HaskellPackageInfo].caches
    pkg_names += d[HaskellPackageInfo].names
    dep_dyn_lib_dirs += d[HaskellPackageInfo].dynamic_libraries

  for n in dep_info.names + depset(ctx.attr.prebuilt_dependencies):
    args.add(["-package", n])

  for c in pkg_caches:
    args.add(["-package-db", c.dirname])

  args.add(object_files)

  ctx.actions.run(
    inputs =
      depset(object_files) +
      pkg_caches +
      dep_dyn_lib_dirs,
    outputs = [dynamic_library_dir, dynamic_library],
    use_default_shell_env = True,
    executable = "ghc",
    arguments = [args]
  )

  return dynamic_library_dir, dynamic_library

def create_ghc_package(ctx, interface_files, interfaces_dir, static_library, static_library_dir,
                       dynamic_library_dir):
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
  conf_file = ctx.actions.declare_file(path_append(pkg_db_dir.basename, "{0}.conf".format(get_pkg_id(ctx))))
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
    "import-dirs": path_append("${pkgroot}", interfaces_dir.basename),
    "library-dirs": path_append("${pkgroot}", static_library_dir.basename),
    "dynamic-library-dirs":
      path_append("${pkgroot}", dynamic_library_dir.basename),
    "hs-libraries": get_library_name(ctx),
    "depends":
      ", ".join([ d[HaskellPackageInfo].name for d in ctx.attr.deps ])
  }
  ctx.actions.write(
    output=registration_file,
    content="\n".join(['{0}: {1}'.format(k, v)
                       for k, v in registration_file_entries.items()])
  )

  pkg_confs = depset([
    c for dep in ctx.attr.deps
      for c in dep[HaskellPackageInfo].confs
  ])

  # Make the call to ghc-pkg and use the registration file
  package_path = ":".join([ c.dirname for c in pkg_confs ])
  ctx.actions.run_shell(
    inputs = pkg_confs + [static_library, interfaces_dir, registration_file],
    outputs = [pkg_db_dir, conf_file, cache_file],
    # TODO: use env for GHC_PACKAGE_PATH when use_default_shell_env is removed
    use_default_shell_env = True,
    command = " ".join(
      [ "GHC_PACKAGE_PATH={0}".format(package_path),
        "ghc-pkg",
        "register",
        "--package-db={0}".format(pkg_db_dir.path),
        "--no-expand-pkgroot",
        registration_file.path,
      ]
    )
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
  """Walk dependencies and collapse their information into a single
  HaskellPackageInfo. "name", "prebuilt_dependencies" and
  "external_libraries" fields are fully pre-populated.

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
    external_libraries = {}
  )

  for dep in ctx.attr.deps:
    pkg = dep[HaskellPackageInfo]
    new_external_libraries = {}
    new_external_libraries.update(hpi.external_libraries)
    new_external_libraries.update(pkg.external_libraries)
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
      external_libraries = new_external_libraries
    )

  # Add external_libraries from the user, formatting them in way code
  # expects.
  for name, f in ctx.attr.external_libraries.items():
    hpi.external_libraries[name] = hpi.external_libraries.get(name, default=depset()) + [f]

  return hpi
