"""GHCi REPL support"""

load(":tools.bzl",
     "tools",
)

load("@bazel_skylib//:lib.bzl",
     "paths",
     "shell",
)

load(":providers.bzl",
     "HaskellBuildInfo",
     "HaskellLibraryInfo",
     "HaskellBinaryInfo",
)

load(":path_utils.bzl",
     "target_unique_name",
     "get_external_libs_path",
     "import_hierarchy_root",
)

load(":set.bzl",
     "set",
)

load(":utils.bzl",
     "get_lib_name",
)

def build_haskell_repl(
    ctx,
    build_info,
    target_files,
    lib_info=None,
    bin_info=None):
  """Build REPL script.

  Args:
    ctx: Rule context.
    build_info: HaskellBuildInfo.

    lib_info: If we're building REPL for a library target, pass
              HaskellLibraryInfo here, otherwise it should be None.
    bin_info: If we're building REPL for a binary target, pass
              HaskellBinaryInfo here, otherwise it should be None.

    target_files: Output files of the target we're generating REPL for.
                  These are passed so we can force their building on REPL
                  building.

  Returns:
    None.
  """

  interpreted = ctx.attr.repl_interpreted

  # Bring packages in scope.
  args = ["-hide-all-packages"]
  for dep in set.to_list(build_info.prebuilt_dependencies):
    args += ["-package ", dep]
  for package in set.to_list(build_info.package_ids):
    if not (interpreted and lib_info != None and package == lib_info.package_id):
      args += ["-package-id", package]
  for cache in set.to_list(build_info.package_caches):
    args += ["-package-db", cache.dirname]

  # Specify import directory for library in interpreted mode.
  if interpreted and lib_info != None:
    for idir in set.to_list(lib_info.import_dirs):
      args += ["-i{0}".format(idir)]

  # External libraries.
  seen_libs = set.empty()
  for lib in build_info.external_libraries.values():
    lib_name = get_lib_name(lib)
    if not set.is_member(seen_libs, lib_name):
      set.mutable_insert(seen_libs, lib_name)
      args += [
        "-l{0}".format(lib_name),
        "-L{0}".format(paths.dirname(lib.path)),
      ]

  ghci_script = ctx.actions.declare_file(target_unique_name(ctx, "ghci-repl-script"))
  repl_file = ctx.actions.declare_file(target_unique_name(ctx, "repl"))

  add_modules = []
  if lib_info != None:
    # If we have a library, we put names of its exposed modules here but
    # only if we're in interpreted mode.
    add_modules = set.to_list(
      lib_info.exposed_modules if interpreted else set.empty()
    )
  elif bin_info != None:
    # Otherwise we put paths to module files, mostly because it also works
    # and Main module may be in a file with name that's impossible for GHC
    # to infer.
    add_modules = [f.path for f in set.to_list(bin_info.source_files)]

  visible_modules = []
  if lib_info != None:
    # If we have a library, we put names of its exposed modules here.
    visible_modules = set.to_list(lib_info.exposed_modules)
  elif bin_info != None:
    # Otherwise we do rougly the same by using modules from
    # HaskellBinaryInfo.
    visible_modules = set.to_list(bin_info.modules)

  ctx.actions.expand_template(
    template = ctx.file._ghci_script,
    output = ghci_script,
    substitutions = {
      "{ADD_MODULES}": " ".join(add_modules),
      "{VISIBLE_MODULES}": " ".join(visible_modules),
    },
  )

  args += ["-ghci-script", ghci_script.path]

  # Extra arguments.
  args += ctx.attr.repl_ghci_args

  ctx.actions.expand_template(
    template = ctx.file._ghci_repl_wrapper,
    output = repl_file,
    substitutions = {
      # XXX I'm not 100% sure if this is necessary, I think it may be
      # necessary for dynamic Haskell libraries to see other dynamic Haskell
      # libraries they are linked with.
      "{LDLIBPATH}": get_external_libs_path(
        set.union(
          build_info.dynamic_libraries,
          set.from_list(build_info.external_libraries.values()),
        )
      ),
      "{GHCi}": tools(ctx).ghci.path,
      "{SCRIPT_LOCATION}": ctx.outputs.repl.path,
      "{ARGS}": " ".join([shell.quote(a) for a in args]),
    },
    is_executable = True,
  )

  relative_target = paths.relativize(repl_file.path, ctx.outputs.repl.dirname)

  ctx.actions.run(
    inputs = depset(transitive = [
      # XXX We create a symlink here because we need to force
      # tools(ctx).ghci and ghci_script and the best way to do that is to
      # use ctx.actions.run. That action, it turn must produce a result, so
      # using ln seems to be the only sane choice.
      depset([tools(ctx).ghci, ghci_script, repl_file]),
      target_files,
    ]),
    outputs = [ctx.outputs.repl],
    executable = tools(ctx).ln,
    arguments = ["-s", relative_target, ctx.outputs.repl.path],
  )
