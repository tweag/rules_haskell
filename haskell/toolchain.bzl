load(":path_utils.bzl",
     "drop_path_prefix",
     "get_dyn_interface_suffix",
     "get_dyn_object_suffix",
     "get_interface_suffix",
     "get_object_suffix",
     "path_append",
     "replace_ext",
     "path_to_module",
     "declare_compiled",
)

HaskellPackageInfo = provider(
  doc = "Package information exposed by Haskell libraries.",
  fields = {
    "pkgName": "Package name, usually of the form name-version.",
    "pkgNames": "All package names of transitive dependencies. Includes own name.",
    "pkgConfs": "Package conf files.",
    "pkgCaches": "Package cache files.",
    "pkgLibs": "Compiled library archives.",
    "pkgDynLibs": "Dynamic libraries.",
    "interfaceFiles": "Interface files belonging to the packages.",
    "pkgImportDirs": "Interface file directories",
    "pkgLibDirs": "Library file directories",
    "pkgDynLibDirs": "Dynamic library file directories",
    "prebuiltDeps": "Transitive collection of all wired-in Haskell dependencies.",
    "externalLibs": "Non-Haskell libraries needed for linking. Dict of Name:LinkDir.",
  }
)

def mk_name(ctx, namePrefix):
  """Make a target-unique name.

  Args:
    namePrefix: Template for the name.
  """
  return "{0}-{1}-{2}".format(namePrefix, ctx.attr.name, ctx.attr.version)

def ghc_bin_obj_args(ctx, objDir):
  """Build arguments for Haskell binary object building.

  Args:
    ctx: Rule context.
    objDir: Output directory for object files.
  """
  args = ctx.actions.args()
  args.add(ctx.attr.compilerFlags)
  args.add("-no-link")
  args.add(ctx.files.srcs)
  args.add(["-odir", objDir])
  args.add(["-main-is", ctx.attr.main])
  args.add(["-osuf", get_object_suffix(), "-hisuf", get_interface_suffix()])

  # Collapse all library dependencies
  depNames = depset([ n for d in ctx.attr.deps for n in d[HaskellPackageInfo].pkgNames ])
  depConfs = depset([ c.dirname for d in ctx.attr.deps for c in d[HaskellPackageInfo].pkgConfs ])

  for n in depNames:
    args.add(["-package", n])
  for db in depConfs:
    args.add(["-package-db", db])
  return args

def ghc_bin_link_args(ctx, binObjs, depLibs, prebuiltDeps, externalLibs):
  """Build arguments for Haskell binary linking stage.

  Also creates an empty library archive to as a build target: this
  stops GHC from complaining about no target when we only want to use
  it for linking. This result is silently passed into the arguments
  but the link target should be explicitly added to the action as an
  input.

  Args:
    ctx: Rule context.
    binObjs: Object files to include during linking.
    depLibs: Library archives to include during linking.

  """
  # Create empty archive so that GHC has some input files to work on during linking
  #
  # https://github.com/facebook/buck/blob/126d576d5c07ce382e447533b57794ae1a358cc2/src/com/facebook/buck/haskell/HaskellDescriptionUtils.java#L295
  dummy = ctx.actions.declare_file("BazelDummy.hs")
  dummyObj = ctx.actions.declare_file("BazelDummy." + get_object_suffix())
  ctx.actions.write(output=dummy, content="module BazelDummy () where")
  dummyLib = ctx.actions.declare_file("libempty.a")
  dummyArgs = ctx.actions.args()
  dummyArgs.add(["-no-link", dummy, "-osuf", get_object_suffix()])
  ctx.actions.run(
    inputs = [dummy],
    outputs = [dummyObj],
    use_default_shell_env = True,
    executable = "ghc",
    arguments = [dummyArgs]
  )
  ctx.actions.run(
    inputs = [dummyObj],
    outputs = [dummyLib],
    use_default_shell_env = True,
    executable = "ar",
    arguments = [ar_args(ctx, dummyLib, [dummyObj])]
  )

  args = ctx.actions.args()

  args.add(["-o", ctx.outputs.executable])
  args.add(dummyLib)
  for o in binObjs:
    args.add(["-optl", o])
  for l in depLibs:
    args.add(["-optl", l])

  for depName, depDirs in externalLibs.items():
    args.add("-l{0}".format(depName))
    for d in depDirs:
      args.add("-L{0}".format(d))

  # We have to remember to specify all (transitive) wired-in
  # dependencies or we can't find objects for linking.
  for p in prebuiltDeps:
    args.add(["-package", p])

  return dummyLib, args

def compile_haskell_lib(ctx, generated_hs_sources):
  """Build arguments for Haskell package build.

  Args:
    ctx: Rule context.
    generated_hs_sources: Generated Haskell files to include in the build.
  """

  objects_dir = ctx.actions.declare_directory(mk_name(ctx, "objects"))
  interfaces_dir = ctx.actions.declare_directory(mk_name(ctx, "interfaces"))
  args = ctx.actions.args()
  args.add(ctx.attr.compilerFlags)
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


  pkg_caches = depset()
  pkg_names = depset()
  dep_interface_files = depset()
  dep_dyn_libs = depset()
  for d in ctx.attr.deps:
    pkg_caches += d[HaskellPackageInfo].pkgCaches
    pkg_names += d[HaskellPackageInfo].pkgNames
    dep_interface_files += d[HaskellPackageInfo].interfaceFiles
    dep_dyn_libs += d[HaskellPackageInfo].pkgDynLibs

  for n in pkg_names + depset(ctx.attr.prebuiltDeps):
    args.add(["-package", n])

  # Only include package DBs for deps, prebuilt deps should be found
  # auto-magically by GHC.
  for c in pkg_caches:
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
    (pkg_caches + external_files + dep_interface_files +
     dep_dyn_libs).to_list(),
    outputs = [interfaces_dir, objects_dir] + object_files + interface_files +
    object_dyn_files,
    use_default_shell_env = True,
    progress_message = "Compiling {0}".format(ctx.attr.name),
    executable = "ghc",
    arguments = [args]
  )

  return interfaces_dir, interface_files, object_files, object_dyn_files

def create_static_library(ctx, library_name, object_files):
  """Create a static library for the package using given object files.

  Args:
    ctx: Rule context.
    object_files: All object files to include in the library.
  """
  args = ctx.actions.args()
  static_library_dir = ctx.actions.declare_directory(mk_name(ctx, "lib"))
  static_library = ctx.actions.declare_file(path_append(static_library_dir.basename, "lib{0}.a".format(library_name)))


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

def create_dynamic_library(ctx, library_name, external_libraries, object_files):
  """Create a dynamic library for the package using given object files.

  Args:
    ctx: Rule context.
    library_name: Name of the library.
    external_libraries: Any absolute path libraries to link against.
    object_files: Object files to use for linking.
  """

  # Make shared library
  dynamic_library_dir = ctx.actions.declare_directory(mk_name(ctx, "dynlib"))
  dynamic_library = ctx.actions.declare_file(
    path_append(dynamic_library_dir.basename,
                "lib{0}-ghc{1}.so".format(library_name, ctx.attr.ghcVersion))
  )

  args = ctx.actions.args()
  args.add(["-shared", "-dynamic", "-o", dynamic_library])

  # We do not add anything to inputs from these as these are supposed
  # to be absolute paths outside of bazel's control such as system
  # libraries.
  for dep_name, dep_dirs in external_libraries.items():
    args.add("-l{0}".format(dep_name))
    for d in dep_dirs:
      args.add("-L{0}".format(d))

  pkg_caches = depset()
  pkg_names = depset()
  dep_dyn_lib_dirs = depset()
  for d in ctx.attr.deps:
    pkg_caches += d[HaskellPackageInfo].pkgCaches
    pkg_names += d[HaskellPackageInfo].pkgNames
    dep_dyn_lib_dirs += d[HaskellPackageInfo].pkgDynLibDirs

  for n in pkg_names + depset(ctx.attr.prebuiltDeps):
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


def get_input_files(ctx):
  """Get all files we expect to project object files from.

  Args:
    ctx: Rule context.
  """
  return ctx.files.srcs + ctx.files.hscs + ctx.files.cpphs

def mk_registration_file(ctx, pkgId, interfaceDir, libDir, dynLibDir, inputFiles, libName):
  """Prepare a file we'll use to register a package with.

  Args:
    ctx: Rule context.
    pkgId: Package ID, usually in name-version format.
    interfaceDir: Directory with interface files.
    libDir: Directory containing library archive(s).
    dynLibDir: Directory containing shared library.
    libName: Shared library name.
  """
  registrationFile = ctx.actions.declare_file(mk_name(ctx, "registration-file"))
  modules = [path_to_module(ctx, f) for f in inputFiles]
  registrationFileDict = {
    "name": ctx.attr.name,
    "version": ctx.attr.version,
    "id": pkgId,
    "key": pkgId,
    "exposed": "True",
    # Translate module source paths in Haskell modules. Best effort
    # without GHC help.
    "exposed-modules": " ".join(modules),
    "import-dirs": path_append("${pkgroot}", interfaceDir.basename),
    "library-dirs": path_append("${pkgroot}", libDir.basename),
    "dynamic-library-dirs": path_append("${pkgroot}", dynLibDir.basename),
    "hs-libraries": libName,
    "depends": ", ".join([ d[HaskellPackageInfo].pkgName for d in ctx.attr.deps ])
  }
  ctx.actions.write(
    output=registrationFile,
    content="\n".join(['{0}: {1}'.format(k, v)
                       for k, v in registrationFileDict.items()])
  )
  return registrationFile

def register_package(registrationFile, pkgDbDir, pkgConfs):
  """Initialises, registers and checks ghc DB package.

  Args:
    registrationFile: File containing package description.
    pkgDbDir: Directory for GHC package DB.
    pkgConfs: Package config files of dependencies this package needs.
  """
  # TODO: Set GHC_PACKAGE_PATH with ctx.actions.run_shell.env when we
  # stop using use_default_shell_env! That way we can't forget to set
  # this.
  packagePath = ":".join([ conf.dirname for conf in pkgConfs ])
  ghcPackagePath = "GHC_PACKAGE_PATH={0}".format(packagePath)

  return " ".join(
    [ ghcPackagePath,
      "ghc-pkg",
      "register",
      "--package-db={0}".format(pkgDbDir.path),
      "--no-expand-pkgroot",
      registrationFile.path,
    ]
  )

# We might want ranlib like Buck does.
def ar_args(ctx, pkgLib, objectFiles):
  """Create arguments for `ar` tool.

  Args:
    pkgLib: The declared static library to generate.
    objectFiles: Object files to pack into the library.
  """
  args = ctx.actions.args()
  args.add("qc")
  args.add(pkgLib)
  args.add(objectFiles)
  return args
