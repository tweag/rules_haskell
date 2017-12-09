load(":toolchain.bzl",
     "HaskellPackageInfo",
     "ar_args",
     "get_dyn_interface_suffix",
     "get_dyn_object_suffix",
     "get_interface_suffix",
     "get_object_suffix",
     "ghc_bin_link_args",
     "ghc_bin_obj_args",
     "ghc_c_dyn_lib_args",
     "ghc_c_lib_args",
     "ghc_cpphs_args",
     "ghc_dyn_link_args",
     "ghc_lib_args",
     "hsc2hs_args",
     "mk_name",
     "mk_registration_file",
     "path_append",
     "register_package",
     "replace_ext",
     "src_to_ext",
)

def _haskell_binary_impl(ctx):
  depInputs = depset()
  depLibs = depset()
  depDynLibs = depset()
  externalLibs = {}
  for d in ctx.attr.deps:
    depInputs += d.files
    pkg = d[HaskellPackageInfo]
    depInputs += pkg.pkgConfs
    depInputs += pkg.pkgCaches
    depInputs += pkg.pkgLibs
    depInputs += pkg.interfaceFiles
    depLibs += pkg.pkgLibs
    depDynLibs += pkg.pkgDynLibs
    externalLibs.update(pkg.externalLibs)

  depInputs += ctx.files.srcs

  objDir = ctx.actions.declare_directory(mk_name(ctx, "objects"))
  binObjs = [ctx.actions.declare_file(src_to_ext(ctx, s, get_object_suffix(ctx), directory=objDir))
             for s in ctx.files.srcs]

  # Compile sources of the binary.
  ctx.actions.run(
    inputs = depInputs + depLibs + depDynLibs,
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

  linkTarget, linkArgs = ghc_bin_link_args(ctx, binObjs, depLibs, prebuiltDeps, externalLibs)
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
  # Directory for object files generated from C files. This can't be
  # the same as objDir as we can have Foo/Bar.hs and Foo/Bar.c and we
  # need to link objects from both without them generating the same
  # file.
  objDirC = ctx.actions.declare_directory(mk_name(ctx, "objects-c"))
  objDirCDyn = ctx.actions.declare_directory(mk_name(ctx, "objects-c-dyn"))
  inputFiles = ctx.files.srcs + ctx.files.hscs + ctx.files.cpphs
  objectFiles = [ctx.actions.declare_file(src_to_ext(ctx, s, get_object_suffix(ctx), directory=objDir))
                 for s in inputFiles]
  cObjectFiles = [ctx.actions.declare_file(path_append(objDirC.basename, replace_ext(s.path, get_object_suffix(ctx))))
                  for s in ctx.files.c_sources]

  dynObjectFiles = [ctx.actions.declare_file(src_to_ext(ctx, s, get_dyn_object_suffix(), directory=objDir))
                    for s in inputFiles]
  cDynObjectFiles = [ctx.actions.declare_file(path_append(objDirCDyn.basename, replace_ext(s.path, get_dyn_object_suffix())))
                     for s in ctx.files.c_sources]

  ifaceDir = ctx.actions.declare_directory(mk_name(ctx, "interfaces"))
  interfaceFiles = [ctx.actions.declare_file(src_to_ext(ctx, s, get_interface_suffix(ctx), directory=ifaceDir))
                    for s in inputFiles]

  # Build transitive depsets
  depPkgConfs = depset()
  depPkgCaches = depset()
  depPkgNames = depset()
  depPkgLibs = depset()
  depPkgDynLibs = depset()
  depInterfaceFiles = depset()
  depImportDirs = depset()
  depLibDirs = depset()
  depDynLibDirs = depset()
  depPrebuiltDeps = depset()
  allExternalLibs = {}
  for d in ctx.attr.deps:
    pkg = d[HaskellPackageInfo]
    depPkgConfs += pkg.pkgConfs
    depPkgCaches += pkg.pkgCaches
    depPkgNames += pkg.pkgNames
    depInterfaceFiles += pkg.interfaceFiles
    depPkgLibs += pkg.pkgLibs
    depPkgDynLibs += pkg.pkgDynLibs
    depImportDirs += pkg.pkgImportDirs
    depLibDirs += pkg.pkgLibDirs
    depDynLibDirs += pkg.pkgDynLibDirs
    depPrebuiltDeps += pkg.prebuiltDeps
    allExternalLibs.update(pkg.externalLibs)

  # Put external libraries for current target into the external lib
  # dict.
  for name, f in ctx.attr.external_libraries.items():
    allExternalLibs[name] = allExternalLibs.get(name, default=depset()) + [f]

  exFiles = depset()
  for t in ctx.attr.external_deps:
    exFiles += t.files

  # Directories of external dependencies.
  includeDirs = depset([ d.dirname for d in exFiles ])

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

  # Compile C static objects
  ctx.actions.run(
    inputs =
    ctx.files.c_sources +
    (depPkgDynLibs + depDynLibDirs + exFiles + depPkgConfs + depPkgLibs +
     depPkgCaches + depInterfaceFiles).to_list(),
    outputs = [objDirC] + cObjectFiles,
    use_default_shell_env = True,
    progress_message = "Compiling C static {0}".format(ctx.attr.name),
    executable = "ghc",
    arguments = [ghc_c_lib_args(ctx, objDirC, depPkgConfs, depPkgNames, includeDirs)],
  )

  # Compile C dynamic objects
  ctx.actions.run(
    inputs =
      ctx.files.c_sources +
      (depPkgDynLibs + depDynLibDirs + exFiles + depPkgConfs + depPkgLibs +
       depPkgCaches + depInterfaceFiles).to_list(),
    outputs = [objDirCDyn] + cDynObjectFiles,
    use_default_shell_env = True,
    progress_message = "Compiling C dynamic {0}".format(ctx.attr.name),
    executable = "ghc",
    arguments = [ghc_c_dyn_lib_args(ctx, objDirCDyn, depPkgConfs, depPkgNames, includeDirs)]
  )

  ctx.actions.run(
    inputs =
      ctx.files.srcs + genHsFiles +
      (depPkgDynLibs + depDynLibDirs + exFiles + depPkgConfs + depPkgLibs +
       depPkgCaches + depInterfaceFiles).to_list(),
    outputs = [ifaceDir, objDir] + objectFiles + interfaceFiles +
              dynObjectFiles,
    use_default_shell_env = True,
    progress_message = "Compiling {0}".format(ctx.attr.name),
    executable = "ghc",
    arguments = [ghc_lib_args(ctx, objDir, ifaceDir, depPkgConfs, depPkgNames, genHsFiles)]
  )

  # Make static library archive
  pkgId = "{0}-{1}".format(ctx.attr.name, ctx.attr.version)
  # Haskell lib names have to start with HS
  # https://ghc.haskell.org/trac/ghc/ticket/9625
  libName = "HS{0}".format(pkgId)
  libDir = ctx.actions.declare_directory(mk_name(ctx, "lib"))
  # We need libDir because ghc-pkg wants a directory for library-dirs.
  pkgLib = ctx.actions.declare_file(path_append(libDir.basename, "lib{0}.a".format(libName)))

  ctx.actions.run(
    inputs = objectFiles + cObjectFiles,
    outputs = [pkgLib, libDir],
    use_default_shell_env = True,
    executable = "ar",
    arguments = [ar_args(ctx, pkgLib, objectFiles + cObjectFiles)],
  )

  # Make shared library
  dynLibDir = ctx.actions.declare_directory(mk_name(ctx, "dynlib"))
  pkgDynLib = ctx.actions.declare_file(path_append(dynLibDir.basename, "lib{0}-ghc{1}.so".format(libName, ctx.attr.ghcVersion)))

  ctx.actions.run(
    inputs = depPkgConfs + depPkgCaches + dynObjectFiles + cDynObjectFiles +
             depDynLibDirs,
    outputs = [pkgDynLib, dynLibDir],
    use_default_shell_env = True,
    executable = "ghc",
    arguments = [ghc_dyn_link_args(ctx, dynObjectFiles + cDynObjectFiles, pkgDynLib, depPkgNames, depPkgConfs, allExternalLibs)]
  )

  # Create and register ghc package.
  pkgDbDir = ctx.actions.declare_directory(pkgId)
  confFile = ctx.actions.declare_file(path_append(pkgDbDir.basename, "{0}.conf".format(pkgId)))
  cacheFile = ctx.actions.declare_file("package.cache", sibling=confFile)
  registrationFile = mk_registration_file(ctx, pkgId, ifaceDir, libDir, dynLibDir, inputFiles, libName)

  ctx.actions.run_shell(
    inputs =
      [ pkgLib, pkgDynLib, ifaceDir, registrationFile ] +
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
    pkgDynLibs = depset([pkgDynLib]) + depPkgDynLibs,
    interfaceFiles = depInterfaceFiles + depset(interfaceFiles),
    pkgImportDirs = depImportDirs + depset([ifaceDir]),
    pkgLibDirs = depLibDirs + depset([libDir]),
    pkgDynLibDirs = depLibDirs + depset([dynLibDir]),
    prebuiltDeps = depPrebuiltDeps + depset(ctx.attr.prebuiltDeps),
    externalLibs = allExternalLibs
  )]

_haskell_common_attrs = {
  "sourceDir": attr.string(
    mandatory=False,
    doc="Directory in which module hierarchy starts."
  ),
  "srcs": attr.label_list(
    allow_files=FileType([".hs"]),
    # TODO: Figure out how to deal with sources where module hierarchy
    # doesn't start straight away.
    doc="A list of Haskell sources to be built by this rule."
  ),
  "c_sources": attr.label_list(
    allow_files=FileType([".c"]),
    doc="A list of C source files to be built as part of the package."
  ),
  "c_options": attr.string_list(
    doc="Options to pass to C compiler for any C source files."
  ),
  "deps": attr.label_list(
    doc="haskell_library dependencies"
  ),
  "compilerFlags": attr.string_list(
    doc="Flags to pass to Haskell compiler while compiling this rule's sources."
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
  # Only supports one per lib for now
  "external_libraries": attr.string_dict(
    doc="Non-Haskell libraries that we should link",
  ),
  "prebuiltDeps": attr.string_list(
    doc="Haskell packages which are magically available such as wired-in packages."
  ),
  "version": attr.string(
    default="1.0.0",
    doc="Package/binary version"
  ),
  "ghcVersion": attr.string(
    default="8.0.2",
    # TODO (fuuzetsu): We need this because we have to generate
    # correct suffix for shared libraries that GHC expects for
    # dynamic-library-dirs content. As currently we're using GHC from
    # nix, there's not really a way to do this. In future we need to
    # expose toolchains that expose a version and use that. I think.
    doc="Version of GHC used."
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
