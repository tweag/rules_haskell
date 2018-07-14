"""GHCi REPL support"""

load("@bazel_skylib//:lib.bzl",
     "paths",
     "shell",
)

load(":private/providers.bzl",
     "HaskellBuildInfo",
     "HaskellLibraryInfo",
     "HaskellBinaryInfo",
)

load(":private/path_utils.bzl",
     "get_lib_name",
     "get_external_libs_path",
     "ln",
     "target_unique_name",
)

load(":private/set.bzl",
     "set",
)

def build_haskell_repl(
    hs,
    ghci_script,
    ghci_repl_wrapper,
    compiler_flags,
    repl_ghci_args,
    build_info,
    target_files,
    interpreted,
    output,
    lib_info=None,
    bin_info=None):
  """Build REPL script.

  Args:
    hs: Haskell context.
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
      args += ["-l{0}".format(lib_name)]

  ghci_repl_script = hs.actions.declare_file(target_unique_name(hs, "ghci-repl-script"))
  repl_file = hs.actions.declare_file(target_unique_name(hs, "repl"))

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

  hs.actions.expand_template(
    template = ghci_script,
    output = ghci_repl_script,
    substitutions = {
      "{ADD_MODULES}": " ".join(add_modules),
      "{VISIBLE_MODULES}": " ".join(visible_modules),
    },
  )

  args += ["-ghci-script", ghci_repl_script.path]

  # Extra arguments.
  # `compiler flags` is the default set of arguments for the repl,
  # augmented by `repl_ghci_args`.
  # The ordering is important, first compiler flags (from toolchain
  # and local rule), then from `repl_ghci_args`. This way the more
  # specific arguments are listed last, and then have more priority in
  # GHC.
  # Note that most flags for GHCI do have their negative value, so a
  # negative flag in `repl_ghci_args` can disable a positive flag set
  # in `compiler_flags`, such as `-XNoOverloadedStrings` will disable
  # `-XOverloadedStrings`.
  args += hs.toolchain.compiler_flags + compiler_flags + hs.toolchain.repl_ghci_args + repl_ghci_args

  hs.actions.expand_template(
    template = ghci_repl_wrapper,
    output = repl_file,
    substitutions = {
      "{LDLIBPATH}": get_external_libs_path(
        set.union(
          build_info.dynamic_libraries,
          set.from_list(build_info.external_libraries.values()),
        ),
        prefix = "$RULES_HASKELL_EXEC_ROOT",
      ),
      "{GHCi}": hs.tools.ghci.path,
      "{SCRIPT_LOCATION}": output.path,
      "{ARGS}": " ".join([shell.quote(a) for a in args]),
    },
    is_executable = True,
  )

  # XXX We create a symlink here because we need to force
  # hs.tools.ghci and ghci_script and the best way to do that is
  # to use hs.actions.run. That action, it turn must produce
  # a result, so using ln seems to be the only sane choice.
  extra_inputs = depset(
    transitive = [
      depset([hs.tools.ghci, ghci_repl_script, repl_file]),
      target_files,
    ])
  ln(hs, repl_file, output, extra_inputs)
