load(":toolchain.bzl",
     "HaskellPackageInfo",
     "ar_args",
     "ghc_bin_link_args",
     "ghc_bin_obj_args",
     "ghc_lib_args",
     "mk_registration_file",
     "register_package",
     "src_to_ext",
)

def _haskell_binary_impl(ctx):
  depInputs = []
  systemLibs = []
  for d in ctx.attr.deps:
    # depend on output of deps, i.e. package file
    depInputs += d.files.to_list()
    # We need interface files of the package for compilation
    depInputs += d[HaskellPackageInfo].interfaceFiles
    # Lastly we need library object for linking
    systemLibs.append(d[HaskellPackageInfo].systemLib)

  objDir = ctx.actions.declare_directory("objects")
  binObjs = [ctx.actions.declare_file(src_to_ext(ctx, s, "o", directory=objDir))
             for s in ctx.files.srcs]

  # Compile sources of the binary.
  ctx.actions.run(
    inputs = ctx.files.srcs + depInputs + systemLibs,
    outputs = binObjs + [objDir],
    use_default_shell_env = True,
    progress_message = "Building {0}".format(ctx.attr.name),
    executable = "ghc",
    arguments = [ghc_bin_obj_args(ctx, objDir)],
    )

  # Link everything together
  linkTarget, linkArgs = ghc_bin_link_args(ctx, binObjs, systemLibs)
  ctx.actions.run(
    inputs = binObjs + systemLibs + [linkTarget],
    outputs = [ctx.outputs.executable],
    use_default_shell_env = True,
    progress_message = "Linking {0}".format(ctx.outputs.executable),
    executable = "ghc",
    arguments = [linkArgs],
  )

def _haskell_library_impl(ctx):

  objDir = ctx.actions.declare_directory("objects")
  objectFiles = [ctx.actions.declare_file(src_to_ext(ctx, s, "o", directory=objDir))
                 for s in ctx.files.srcs]

  ifaceDir = ctx.actions.declare_directory("interfaces")
  interfaceFiles = [ctx.actions.declare_file(src_to_ext(ctx, s, "hi", directory=ifaceDir))
                    for s in ctx.files.srcs ]

  # Compile library files
  #
  # TODO: Library deps
  ctx.actions.run(
    inputs = ctx.files.srcs,
    outputs = [ifaceDir, objDir] + objectFiles + interfaceFiles,
    use_default_shell_env = True,
    progress_message = "Compiling {0}".format(ctx.attr.name),
    executable = "ghc",
    arguments = [ghc_lib_args(ctx, objDir, ifaceDir)]
  )

  # Make library archive; currently only static
  #
  # TODO: configurable shared &c. see various scenarios in buck
  libDir = ctx.actions.declare_directory("lib")
  systemLib = ctx.actions.declare_file("{0}/lib{1}.a".format(libDir.basename, ctx.attr.name))

  ctx.actions.run(
    inputs = objectFiles,
    outputs = [systemLib, libDir],
    use_default_shell_env = True,
    executable = "ar",
    arguments = [ar_args(ctx, systemLib, objectFiles)],
  )

  # Create and register ghc package.
  pkgId = "{0}-{1}".format(ctx.attr.name, ctx.attr.version)
  confFile = ctx.actions.declare_file("{0}.conf".format(pkgId))
  cacheFile = ctx.actions.declare_file("package.cache")
  registrationFile = mk_registration_file(ctx, pkgId, ifaceDir, libDir)
  ctx.actions.run_shell(
    inputs = [systemLib, ifaceDir, registrationFile],
    outputs = [confFile, cacheFile],
    use_default_shell_env = True,
    command = register_package(ifaceDir, registrationFile, confFile.dirname)
  )
  return [HaskellPackageInfo( packageName = pkgId,
                              pkgDb = confFile.dirname,
                              systemLib = systemLib,
                              interfaceFiles = interfaceFiles)]

_haskell_common_attrs = {
  "srcs": attr.label_list(allow_files = FileType([".hs"])),
  "deps": attr.label_list(),
}

haskell_library = rule(
  _haskell_library_impl,
  outputs = {
    "conf": "%{name}-%{version}.conf",
    "packageCache": "package.cache"
  },
  attrs = _haskell_common_attrs + {
    "version": attr.string(default="1.0.0"),
  }
)

haskell_binary = rule(
  _haskell_binary_impl,
  executable = True,
  attrs = _haskell_common_attrs,
)
