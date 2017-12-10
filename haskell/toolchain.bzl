load(":path_utils.bzl",
     "drop_path_prefix",
     "get_dyn_interface_suffix",
     "get_dyn_object_suffix",
     "get_interface_suffix",
     "get_object_suffix",
     "path_append",
     "replace_ext",
     "path_to_module",
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

def ghc_c_lib_args(ctx, objDir, pkgConfs, pkgNames, includeDirs):
  args = ctx.actions.args()
  args.add([
    "-c", "-fPIC",
    "-osuf", get_object_suffix(),
    "-odir", objDir
  ])

  args.add("-optc-O2")
  for opt in ctx.attr.c_options:
    args.add("-optc{0}".format(opt))

  # Expose every dependency and every prebuilt dependency.
  packages = pkgNames + depset(ctx.attr.prebuiltDeps)
  for n in packages:
    args.add(["-package", n])

  for c in pkgConfs:
    args.add(["-package-db", c.dirname])

  for d in includeDirs:
    args.add("-I{0}".format(d))

  args.add(ctx.files.c_sources)
  return args

def ghc_c_dyn_lib_args(ctx, objDir, pkgConfs, pkgNames, includeDirs):
  args = ctx.actions.args()
  args.add([
    "-c", "-dynamic", "-fPIC",
    "-osuf", get_dyn_object_suffix(),
    "-odir", objDir
  ])

  args.add("-optc-O2")
  for opt in ctx.attr.c_options:
    args.add("-optc{0}".format(opt))

  # Expose every dependency and every prebuilt dependency.
  packages = pkgNames + depset(ctx.attr.prebuiltDeps)
  for n in packages:
    args.add(["-package", n])

  for c in pkgConfs:
    args.add(["-package-db", c.dirname])

  for d in includeDirs:
    args.add("-I{0}".format(d))

  args.add(ctx.files.c_sources)
  return args

def ghc_lib_args(ctx, objDir, ifaceDir, pkgConfs, pkgNames, genHsFiles):
  """Build arguments for Haskell package build.

  Args:
    ctx: Rule context.
    objDir: Output directory for object files.
    ifaceDir: Output directory for interface files.
    pkgConfs: Package conf files of dependencies.
    pkgNames: Package names of dependencies.
    genHsFiles: Generated Haskell files.
  """
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
    "-odir", objDir, "-hidir", ifaceDir,
  ])

  # Expose every dependency and every prebuilt dependency.
  packages = pkgNames + depset(ctx.attr.prebuiltDeps)
  for n in packages:
    args.add(["-package", n])

  # Only include package DBs for deps, prebuilt deps should be found
  # auto-magically by GHC.
  for c in pkgConfs:
    args.add(["-package-db", c.dirname])

  args.add(genHsFiles)
  args.add(ctx.files.srcs)
  return args

def ghc_dyn_link_args(ctx, dynObjectFiles, pkgDynLib, pkgNames, pkgConfs, allExternalLibs):
  """Build arguments for Haskell package build.

  Args:
    ctx: Rule context.
    dynObjectFiles: Built dynamic object files.
    pkgDynLib: Output location for the shared library.
  """
  args = ctx.actions.args()
  args.add([
    "-shared", "-dynamic",
    "-o", pkgDynLib,
  ])
  for depName, depDirs in allExternalLibs.items():
    args.add("-l{0}".format(depName))
    for d in depDirs:
      args.add("-L{0}".format(d))

  packages = pkgNames + depset(ctx.attr.prebuiltDeps)
  for n in packages:
    args.add(["-package", n])

  for c in pkgConfs:
    args.add(["-package-db", c.dirname])

  args.add(dynObjectFiles)
  return args

def hsc2hs_args(ctx, hscFile, hsOut, includeDirs):
  args = ctx.actions.args()
  args.add(hscFile)
  args.add(["-o", hsOut])
  for includeDir in includeDirs:
    args.add(["-I", includeDir])
  return args

def ghc_cpphs_args(ctx, cpphsFile, hsOut, includeDirs):
  args = ctx.actions.args()
  args.add(["-E", "-cpp", "-pgmPcpphs", "-optP--cpp", "-x", "hs"])
  for includeDir in includeDirs:
    args.add("-optP-I{0}".format(includeDir))

  args.add(["-o", hsOut])
  args.add(cpphsFile)
  return args

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
