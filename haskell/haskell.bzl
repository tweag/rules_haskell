load(":toolchain.bzl",
     "HaskellPackageInfo",
     "ar_args",
     "ghc_bin_link_args",
     "ghc_bin_obj_args",
     "ghc_lib_args",
     "mk_registration_file",
     "register_package",
     "src_to_ext",
     "get_object_suffix",
     "get_interface_suffix",
     "hsc2hs_args",
     "mk_name",
     "ghc_cpphs_args",
)

def _haskell_binary_impl(ctx):
  depInputs = depset()
  depLibs = depset()
  for d in ctx.attr.deps:
    depInputs += d.files
    pkg = d[HaskellPackageInfo]
    depInputs += pkg.pkgConfs
    depInputs += pkg.pkgCaches
    depInputs += pkg.pkgLibs
    depInputs += pkg.interfaceFiles
    depLibs += pkg.pkgLibs

  depInputs += ctx.files.srcs

  objDir = ctx.actions.declare_directory(mk_name(ctx, "objects"))
  binObjs = [ctx.actions.declare_file(src_to_ext(ctx, s, get_object_suffix(ctx), directory=objDir))
             for s in ctx.files.srcs]

  # Compile sources of the binary.
  ctx.actions.run(
    inputs = depInputs + depLibs,
    outputs = binObjs + [objDir],
    # TODO: use env for GHC_PACKAGE_PATH when use_default_shell_env is removed
    use_default_shell_env = True,
    progress_message = "Building {0}".format(ctx.attr.name),
    executable = "ghc",
    arguments = [ghc_bin_obj_args(ctx, objDir)]
    )

  # Link everything together
  prebuiltDeps = depset(ctx.attr.prebuiltDeps)
  for d in ctx.attr.deps:
    prebuiltDeps += d[HaskellPackageInfo].prebuiltDeps

  linkTarget, linkArgs = ghc_bin_link_args(ctx, binObjs, depLibs, prebuiltDeps)
  ctx.actions.run(
    inputs = binObjs + depLibs.to_list() + [linkTarget],
    outputs = [ctx.outputs.executable],
    use_default_shell_env = True,
    progress_message = "Linking {0}".format(ctx.outputs.executable.basename),
    executable = "ghc",
    arguments = [linkArgs],
  )

def _haskell_library_impl(ctx):
  objDir = ctx.actions.declare_directory(mk_name(ctx, "objects"))
  objectFiles = [ctx.actions.declare_file(src_to_ext(ctx, s, get_object_suffix(ctx), directory=objDir))
                 for s in ctx.files.srcs]

  ifaceDir = ctx.actions.declare_directory(mk_name(ctx, "interfaces"))
  interfaceFiles = [ctx.actions.declare_file(src_to_ext(ctx, s, get_interface_suffix(ctx), directory=ifaceDir))
                    for s in ctx.files.srcs ]

  # Build transitive depsets
  depPkgConfs = depset()
  depPkgCaches = depset()
  depPkgNames = depset()
  depPkgLibs = depset()
  depInterfaceFiles = depset()
  depImportDirs = depset()
  depLibDirs = depset()
  depPrebuiltDeps = depset()
  for d in ctx.attr.deps:
    pkg = d[HaskellPackageInfo]
    depPkgConfs += pkg.pkgConfs
    depPkgCaches += pkg.pkgCaches
    depPkgNames += pkg.pkgNames
    depInterfaceFiles += pkg.interfaceFiles
    depPkgLibs += pkg.pkgLibs
    depImportDirs += pkg.pkgImportDirs
    depLibDirs += pkg.pkgLibDirs
    depPrebuiltDeps += pkg.prebuiltDeps

  exFiles = depset()
  for t in ctx.attr.external_deps:
    exFiles += t.files

  # Directories of external dependencies.
  includeDirs = depset()
  for exFile in exFiles:
    includeDirs += depset([exFile.dirname])

  # Process hsc files
  hscFiles = []
  for hscFile in ctx.files.hscs:
    hsOut = ctx.actions.declare_file(src_to_ext(ctx, hscFile, "hs"))
    ctx.actions.run(
      inputs = exFiles + depset([hscFile]),
      outputs = [hsOut],
      use_default_shell_env = True,
      progress_message = "Processing {0}".format(hscFile.basename),
      executable = "hsc2hs",
      arguments = [hsc2hs_args(ctx, hscFile, hsOut, includeDirs)],
    )
    hscFiles.append(hsOut)

  # Process cpphs files
  cpphsFiles = []
  for cpphsFile in ctx.files.cpphs:
    hsOut = ctx.actions.declare_file(src_to_ext(ctx, cpphsFile, "hs"))
    ctx.actions.run(
      inputs = exFiles + depset([cpphsFile]),
      outputs = [hsOut],
      use_default_shell_env = True,
      progress_message = "Processing {0}".format(cpphsFile.basename),
      executable = "ghc",
      arguments = [ghc_cpphs_args(ctx, cpphsFile, hsOut, includeDirs)],
    )
    cpphsFiles.append(hsOut)

  genHsFiles = hscFiles + cpphsFiles

  # Compile library files
  ctx.actions.run(
    inputs =
      ctx.files.srcs + genHsFiles +
      (depPkgConfs + depPkgCaches + depInterfaceFiles).to_list(),
    outputs = [ifaceDir, objDir] + objectFiles + interfaceFiles,
    use_default_shell_env = True,
    progress_message = "Compiling {0}".format(ctx.attr.name),
    executable = "ghc",
    arguments = [ghc_lib_args(ctx, objDir, ifaceDir, depPkgConfs, depPkgNames,
                              genHsFiles)]
  )

  # Make library archive; currently only static
  #
  # TODO: configurable shared &c. see various scenarios in buck
  pkgId = "{0}-{1}".format(ctx.attr.name, ctx.attr.version)
  libDir = ctx.actions.declare_directory(mk_name(ctx, "lib"))
  # We need libDir because ghc-pkg wants a directory for library-dirs.
  pkgLib = ctx.actions.declare_file("{0}/lib{1}.a".format(libDir.basename, pkgId))
  ctx.actions.run(
    inputs = objectFiles,
    outputs = [pkgLib, libDir],
    use_default_shell_env = True,
    executable = "ar",
    arguments = [ar_args(ctx, pkgLib, objectFiles)],
  )

  # Create and register ghc package.
  pkgDbDir = ctx.actions.declare_directory(pkgId)
  confFile = ctx.actions.declare_file("{0}/{1}.conf".format(pkgDbDir.basename, pkgId))
  cacheFile = ctx.actions.declare_file("package.cache", sibling=confFile)
  registrationFile = mk_registration_file(ctx, pkgId, ifaceDir, libDir)

  ctx.actions.run_shell(
    inputs =
      [ pkgLib, ifaceDir, registrationFile ] +
      (depPkgConfs + depPkgCaches + depPkgLibs + depImportDirs + depLibDirs + interfaceFiles).to_list(),
    outputs = [pkgDbDir, confFile, cacheFile],
    # TODO: use env for GHC_PACKAGE_PATH when use_default_shell_env is removed
    use_default_shell_env = True,
    command = register_package(registrationFile, pkgDbDir, depPkgConfs)
  )

  return [HaskellPackageInfo(
    pkgName = pkgId,
    pkgNames = depPkgNames + depset([pkgId]),
    pkgConfs = depPkgConfs + depset ([confFile]),
    pkgCaches = depPkgCaches + depset([cacheFile]),
    # Keep package libraries in preorder (naive_link) order: this
    # gives us the valid linking order at binary linking time.
    pkgLibs = depset([pkgLib], order="preorder") + depPkgLibs,
    interfaceFiles = depInterfaceFiles + depset(interfaceFiles),
    pkgImportDirs = depImportDirs + depset([ifaceDir]),
    pkgLibDirs = depLibDirs + depset([libDir]),
    prebuiltDeps = depPrebuiltDeps + depset(ctx.attr.prebuiltDeps)
  )]

_haskell_common_attrs = {
  "srcs": attr.label_list(
    allow_files=FileType([".hs"]),
    # TODO: Figure out how to deal with sources where module hierarchy
    # doesn't start straight away.
    doc="A list of Haskell sources to be built by this rule."
  ),
  "deps": attr.label_list(
    doc="haskell_library dependencies"
  ),
  "compilerFlags": attr.string_list(
    doc="Flags to pass to Haskell compiler while compiling this rule's sources."
  ),
  "profiling": attr.bool(
    default=False,
    doc="Build target profiled. You need to enable profiling for all the dependencies as well."
  ),
  "PIC": attr.bool(
    default=False,
    doc="Build as position independent code"
  ),
  "hscs": attr.label_list(
    allow_files=FileType([".hsc"]),
    doc=".hsc files to preprocess and link"
  ),
  "cpphs": attr.label_list(
    allow_files=FileType([".cpphs"]),
    doc=".cpphs file to preprocess and link",
  ),
  "external_deps": attr.label_list(
    allow_files=True,
    doc="Non-Haskell dependencies",
  ),
  "prebuiltDeps": attr.string_list(
    doc="Haskell packages which are magically available such as wired-in packages."
  ),
  "version": attr.string(
    default="1.0.0",
    doc="Package/binary version"
  ),
}

haskell_library = rule(
  _haskell_library_impl,
  outputs = {
    "conf": "%{name}-%{version}/%{name}-%{version}.conf",
    "packageCache": "%{name}-%{version}/package.cache"
  },
  attrs = _haskell_common_attrs,
)

haskell_binary = rule(
  _haskell_binary_impl,
  executable = True,
  attrs = _haskell_common_attrs + {
    "main": attr.string(
      default="Main.main",
      doc="Main function location."
    )
  }
)
