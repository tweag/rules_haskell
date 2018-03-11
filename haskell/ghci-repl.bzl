"""GHCi REPL support"""

load(":tools.bzl",
     "get_ghci",
)

load("@bazel_skylib//:lib.bzl",
     "paths",
     "shell",
)

load(":providers.bzl",
     "HaskellPackageInfo",
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

def _haskell_repl_impl(ctx):

  target = ctx.attr.target[HaskellPackageInfo]

  # Bring packages in scope.
  args = ["-hide-all-packages"]
  for dep in set.to_list(target.prebuilt_dependencies):
    args += ["-package ", dep]
  for name in target.names.to_list():
    if not (ctx.attr.interpreted and name == target.name):
      args += ["-package", name]
  for cache in target.caches.to_list():
    args += ["-package-db", cache.dirname]

  # Import dirs in interpreted mode.
  if ctx.attr.interpreted:
    for idir in set.to_list(target.import_dirs):
      args += ["-i{0}".format(idir)]

  # External libraries.
  seen_libs = set.empty()
  for lib in set.to_list(target.external_libraries):
    lib_name = get_lib_name(lib)
    if not set.is_member(seen_libs, lib_name):
      set.mutable_insert(seen_libs, lib_name)
      args += [
        "-l{0}".format(lib_name),
        "-L{0}".format(paths.dirname(lib.path)),
      ]

  ghci_script = ctx.actions.declare_file(target_unique_name(ctx, "ghci-repl-script"))

  interpreted_modules = set.to_list(target.exposed_modules if ctx.attr.interpreted else set.empty())
  visible_modules = set.to_list(target.exposed_modules)

  ctx.actions.expand_template(
    template = ctx.file._ghci_script,
    output = ghci_script,
    substitutions = {
      "{INTERPRETED_MODULES}": " ".join(interpreted_modules),
      "{VISIBLE_MODULES}": " ".join(visible_modules),
    },
  )

  args += ["-ghci-script", ghci_script.path]

  # Extra arguments.
  args += ctx.attr.ghci_args

  ctx.actions.expand_template(
    template = ctx.file._ghci_repl_wrapper,
    output = ctx.outputs.executable,
    substitutions = {
      # XXX I'm not 100% sure if this is necessary, I think it may be
      # necessary for dynamic Haskell libraries to see other dynamic Haskell
      # libraries they are linked with.
      "{LDLIBPATH}": get_external_libs_path(
        set.union(
          target.dynamic_libraries,
          target.external_libraries,
        )
      ),
      "{GHCi}": get_ghci(ctx).path,
      "{SCRIPT_LOCATION}": ctx.outputs.executable.path,
      "{ARGS}": " ".join([shell.quote(a) for a in args]),
    },
    is_executable = True,
  )

  return [DefaultInfo(
    executable = ctx.outputs.executable,
    files = depset([
      ctx.outputs.executable,
      ghci_script,
    ]),
    # This "forces" compilation of target:
    runfiles = ctx.runfiles(ctx.files.target),
  )]

haskell_repl = rule(
  _haskell_repl_impl,
  executable = True,
  attrs = {
    "target": attr.label(
      mandatory = True,
      doc = "Target that should be available in the REPL.",
    ),
    "interpreted": attr.bool(
      default = True,
      doc = """
Whether source files of `target` should interpreted rather than compiled.
This allows for e.g. reloading of sources on editing, but in this case we
don't handle boot files and hsc preprocessing.
"""
    ),
    "ghci_args": attr.string_list(
      doc = "Arbitrary extra arguments to pass to GHCi.",
    ),
    # XXX Consider making this private. Blocked on
    # https://github.com/bazelbuild/bazel/issues/4366.
    "version": attr.string(
      default = "1.0.0",
      doc = "Library/binary version. Internal - do not use."
    ),
    "_ghci_script": attr.label(
      allow_single_file = True,
      default = Label("@io_tweag_rules_haskell//haskell:ghci-script"),
    ),
    "_ghci_repl_wrapper": attr.label(
      allow_single_file = True,
      default = Label("@io_tweag_rules_haskell//haskell:ghci-repl-wrapper.sh"),
    ),
  },
  toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)
"""Produce a script that calls GHCi for working with `target`.

Example of use:

```
$ bazel build //test:my-repl
$ bazel-bin/test/my-repl
```
"""
