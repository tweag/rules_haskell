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
     "get_interface_suffix"
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

  objDir = ctx.actions.declare_directory("objects")
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
  linkTarget, linkArgs = ghc_bin_link_args(ctx, binObjs, depLibs)
  ctx.actions.run(
    inputs = binObjs + depLibs.to_list() + [linkTarget],
    outputs = [ctx.outputs.executable],
    use_default_shell_env = True,
    progress_message = "Linking {0}".format(ctx.outputs.executable.basename),
    executable = "ghc",
    arguments = [linkArgs],
  )

def _haskell_library_impl(ctx):

  objDir = ctx.actions.declare_directory("objects")
  objectFiles = [ctx.actions.declare_file(src_to_ext(ctx, s, get_object_suffix(ctx), directory=objDir))
                 for s in ctx.files.srcs]

  ifaceDir = ctx.actions.declare_directory("interfaces")
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
  for d in ctx.attr.deps:
    pkg = d[HaskellPackageInfo]
    depPkgConfs += pkg.pkgConfs
    depPkgCaches += pkg.pkgCaches
    depPkgNames += pkg.pkgNames
    depInterfaceFiles += pkg.interfaceFiles
    depPkgLibs += pkg.pkgLibs
    depImportDirs += pkg.pkgImportDirs
    depLibDirs += pkg.pkgLibDirs

   # Compile library files
  ctx.actions.run(
    inputs =
      ctx.files.srcs +
      (depPkgConfs + depPkgCaches + depInterfaceFiles).to_list(),
    outputs = [ifaceDir, objDir] + objectFiles + interfaceFiles,
    use_default_shell_env = True,
    progress_message = "Compiling {0}".format(ctx.attr.name),
    executable = "ghc",
    arguments = [ghc_lib_args(ctx, objDir, ifaceDir, depPkgConfs, depPkgNames)]
  )

  # Make library archive; currently only static
  #
  # TODO: configurable shared &c. see various scenarios in buck
  libDir = ctx.actions.declare_directory("lib")
  pkgLib = ctx.actions.declare_file("{0}/lib{1}.a".format(libDir.basename, ctx.attr.name))

  ctx.actions.run(
    inputs = objectFiles,
    outputs = [pkgLib, libDir],
    use_default_shell_env = True,
    executable = "ar",
    arguments = [ar_args(ctx, pkgLib, objectFiles)],
  )

  # Create and register ghc package.
  pkgId = "{0}-{1}".format(ctx.attr.name, ctx.attr.version)
  confFile = ctx.actions.declare_file("{0}.conf".format(pkgId))
  cacheFile = ctx.actions.declare_file("package.cache", sibling=confFile)
  registrationFile = mk_registration_file(ctx, pkgId, ifaceDir, libDir)
  ctx.actions.run_shell(
    inputs =
      [ pkgLib, ifaceDir, registrationFile ] +
      (depPkgConfs + depPkgCaches + depPkgLibs + depImportDirs + depLibDirs + interfaceFiles).to_list(),
    outputs = [confFile, cacheFile],
    # TODO: use env for GHC_PACKAGE_PATH when use_default_shell_env is removed
    use_default_shell_env = True,
    command = register_package(registrationFile, confFile, depPkgConfs)
  )

  return [HaskellPackageInfo(
    pkgName = pkgId,
    pkgNames = depPkgNames + depset([pkgId]),
    pkgConfs = depPkgConfs + depset([confFile]),
    pkgCaches = depPkgCaches + depset([cacheFile]),
    # Keep package libraries in preorder (naive_link) order: this
    # gives us the valid linking order at binary linking time.
    pkgLibs = depset([pkgLib], order="preorder") + depPkgLibs,
    interfaceFiles = depInterfaceFiles + depset(interfaceFiles),
    pkgImportDirs = depImportDirs + depset([ifaceDir]),
    pkgLibDirs = depLibDirs + depset([libDir])
  )]

_haskell_common_attrs = {
  "srcs": attr.label_list(
    allow_files=FileType([".hs"]),
    doc="A list of Haskell sources to be built by this rule."
  ),
  "deps": attr.label_list(
    doc="haskell_library dependencies"
  ),
  "compilerFlags": attr.string_list(
    doc="Flags to pass to Haskell compiler while compiling this rule's sources."
  ),
  "profiling": attr.bool(
    # Disabled because of linking errors against RTS. Buck has exactly
    # the same problem.
    default=False,
    doc="Build target profiled. You need to enable profiling for all the dependencies as well. Broken."
  ),
  "PIC": attr.bool(
    default=False,
    doc="Build as position independent code"
  ),
}

haskell_library = rule(
  _haskell_library_impl,
  outputs = {
    "conf": "%{name}-%{version}.conf",
    "packageCache": "package.cache"
  },
  attrs = _haskell_common_attrs + {
    "version": attr.string(
      default="1.0.0",
      doc="Library version"
    ),
  }
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
