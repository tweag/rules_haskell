HaskellPackageInfo = provider(
  doc = "Package information exposed by Haskell libraries.",
  fields = {
    "pkgName": "Package name, usually of the form name-version.",
    "pkgNames": "All package names of transitive dependencies. Includes own name.",
    "pkgConfs": "Package conf files.",
    "pkgCaches": "Package cache files.",
    "pkgLibs": "Compiled library archives.",
    "interfaceFiles": "Interface files belonging to the packages.",
    "pkgImportDirs": "Interface file directories",
    "pkgLibDirs": "Library file directories",
    "prebuiltDeps": "Transitive collection of all wired-in Haskell dependencies."
  }
)

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

  if ctx.attr.profiling:
    args.add("-prof")
  elif ctx.attr.PIC:
    args.add("-dynamic")
  args.add(["-osuf", get_object_suffix(ctx), "-hisuf", get_interface_suffix(ctx)])

  # Collapse all library dependencies
  depNames = depset([ n for d in ctx.attr.deps for n in d[HaskellPackageInfo].pkgNames ])
  depConfs = depset([ c.dirname for d in ctx.attr.deps for c in d[HaskellPackageInfo].pkgConfs ])

  for n in depNames:
    args.add(["-package", n])
  for db in depConfs:
    args.add(["-package-db", db])
  return args

def ghc_bin_link_args(ctx, binObjs, depLibs, prebuiltDeps):
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
  dummyObj = ctx.actions.declare_file("BazelDummy." + get_object_suffix(ctx))
  ctx.actions.write(output=dummy, content="module BazelDummy () where")
  dummyLib = ctx.actions.declare_file("libempty.a")
  dummyArgs = ctx.actions.args()
  dummyArgs.add(["-no-link", dummy, "-osuf", get_object_suffix(ctx)])
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
  if ctx.attr.profiling:
    args.add("-prof")
  args.add(["-o", ctx.outputs.executable])
  args.add(dummyLib)
  for o in binObjs:
    args.add(["-optl", o])
  for l in depLibs:
    args.add(["-optl", l])

  # We have to remember to specify all (transitive) wired-in
  # dependencies or we can't find objects for linking.
  for p in prebuiltDeps:
    args.add(["-package", p])

  return dummyLib, args

def ghc_lib_args(ctx, objDir, ifaceDir, pkgConfs, pkgNames, hscFiles):
  """Build arguments for Haskell package build.

  Args:
    ctx: Rule context.
    objDir: Output directory for object files.
    ifaceDir: Output directory for interface files.
    pkgConfs: Package conf files of dependencies.
    pkgNames: Package names of dependencies.
    hscFiles: Processed hsc files.
  """
  args = ctx.actions.args()
  args.add(ctx.attr.compilerFlags)
  args.add(["-no-link"])
  args.add(["-package-name", "{0}-{1}".format(ctx.attr.name, ctx.attr.version)])
  args.add(["-odir", objDir, "-hidir", ifaceDir])

  if ctx.attr.profiling:
    args.add("-prof")
  elif ctx.attr.PIC:
    args.add("-dynamic")
  args.add(["-osuf", get_object_suffix(ctx), "-hisuf", get_interface_suffix(ctx)])

  for n in pkgNames:
    args.add(["-package", n])
  for c in pkgConfs:
    args.add(["-package-db", c.dirname])

  args.add(hscFiles)
  args.add(ctx.files.srcs)
  return args

def hsc2hs_args(ctx, hscFile, hsOut, exFiles):
  args = ctx.actions.args()
  args.add("-v")
  args.add(hscFile)
  args.add(["-o", hsOut])
  includeDirs = depset()
  for exFile in exFiles:
    includeDir = exFile.dirname
    if includeDir not in includeDirs:
      args.add(["-I", includeDir])
      includeDirs += depset([includeDir])
  return args

def take_haskell_module(ctx, f):
  """Given Haskell source file, get the path hierarchy without the extension.

  some-workspace/some-package/Foo/Bar/Baz.hs => Foo/Bar/Baz

  Args:
    f: Haskell source file.
  """
  pkgDir = "{0}/{1}/".format(ctx.label.workspace_root, ctx.label.package)
  pkgDirLen = len(pkgDir)
  # TODO: hack; depending on circumstance, workspace_root can have a
  # leading / which f.path does not have: if that's the case, drop one
  # less character. If workspace_root and package are both empty, drop
  # two characters.
  if pkgDir == "//":
    pkgDirLen -= 2
  elif pkgDirLen > 0 and pkgDir[0] == '/':
    pkgDirLen -= 1
  return f.path[pkgDirLen:f.path.rfind(".")]

def mk_registration_file(ctx, pkgId, interfaceDir, libDir):
  """Prepare a file we'll use to register a package with.

  Args:
    ctx: Rule context.
    pkgId: Package ID, usually in name-version format.
    interfaceDir: Directory with interface files.
    libDir: Directory containing library archive(s).
  """
  registrationFile = ctx.actions.declare_file("registration-file")
  registrationFileDict = {
    "name": ctx.attr.name,
    "version": ctx.attr.version,
    "id": pkgId,
    "key": pkgId,
    "exposed": "True",
    # Translate module source paths in Haskell modules. Best effort
    # without GHC help.
    "exposed-modules": " ".join([take_haskell_module(ctx, f).replace("/", ".")
                                 for f in ctx.files.srcs]),
    "import-dirs": "${{pkgroot}}/{0}".format(interfaceDir.basename),
    "library-dirs": "${{pkgroot}}/{0}".format(libDir.basename),
    "hs-libraries": ctx.attr.name,
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

  registerPackage = " ".join(
    [ ghcPackagePath,
      "ghc-pkg",
      "-v0",
      "register",
      "--package-conf={0}".format(pkgDbDir.path),
      "--no-expand-pkgroot",
      "-v",
      registrationFile.path,
    ]
  )

  # make sure what we produce is valid
  checkPackage = "{0}  ghc-pkg check -v --package-conf={1}".format(ghcPackagePath, pkgDbDir.path)
  return " && ".join(
    [ registerPackage,
      checkPackage
    ]
  )

def get_object_suffix(ctx):
  """Get the object file suffix that GHC expects for this mode of compilation.

  Args:
    ctx: Rule context.
  """
  if ctx.attr.profiling:
    return "p_o"
  else:
    return "o"

def get_interface_suffix(ctx):
  """Get the interface file suffix that GHC expects for this mode of
  compilation.

  Args:
    ctx: Rule context.
  """
  if ctx.attr.PIC:
    return "dyn_hi"
  elif ctx.attr.profiling:
    return "p_hi"
  else:
    return "hi"

def src_to_ext(ctx, src, ext, directory=None):
  """Haskell source file to Haskell file living in specified directory.

  Args:
    ctx: Rule context.
    src: Haskell source file.
    ext: New extension.
    directory: Directory the new file should live in.
  """
  fp = "{0}.{1}".format(take_haskell_module(ctx, src), ext)
  if directory == None:
    return fp
  else:
    return "{0}/{1}".format(directory.basename, fp)

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
