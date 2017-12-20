"""C file compilation.
"""

load(":path_utils.bzl",
     "declare_compiled",
     "path_append",
     "replace_ext",
     "get_object_suffix",
     "get_dyn_object_suffix",
)


load(":toolchain.bzl",
     "HaskellPackageInfo",
     "mk_name",
)


def c_compile_static(ctx):
  """Compile all C files to static object files.

  TODO: We would like to compile these one at a time. This is somewhat
  difficult as they all go into the same object directory and
  therefore share prefix. Possibly we don't care about where in the
  hierarchy they go though so we may be able to just put them in
  separate directory each. Or something.

  Args:
    ctx: Rule context.
  """
  args = ctx.actions.args()
  args.add("-c")
  return __generic_c_compile(ctx, "objects_c", get_dyn_object_suffix(), args)

def c_compile_dynamic(ctx):
  """Compile all C files to dynamic object files.

  TODO: We would like to compile these one at a time. This is somewhat
  difficult as they all go into the same object directory and
  therefore share prefix. Possibly we don't care about where in the
  hierarchy they go though so we may be able to just put them in
  separate directory each. Or something.

  Args:
    ctx: Rule context.
  """
  args = ctx.actions.args()
  args.add(["-c", "-dynamic"])
  return __generic_c_compile(ctx, "objects_c_dyn", get_dyn_object_suffix(), args)

def __generic_c_compile(ctx, output_dir_template, output_ext, user_args):
  # Directory for objects generated from C files.
  output_dir = ctx.actions.declare_directory(mk_name(ctx, output_dir_template))

  args = ctx.actions.args()
  args.add([
    "-fPIC",
    "-osuf", output_ext,
    "-odir", output_dir
  ])

  for opt in ctx.attr.c_options:
    args.add("-optc{0}".format(opt))

  pkg_caches = depset()
  pkg_names = depset()
  for d in ctx.attr.deps:
    if HaskellPackageInfo in d:
      pkg_caches += d[HaskellPackageInfo].caches
      pkg_names += d[HaskellPackageInfo].names

  # Expose every dependency and every prebuilt dependency.
  for n in pkg_names + depset(ctx.attr.prebuilt_dependencies):
    args.add(["-package", n])

  # Point at every package DB we depend on and know of explicitly.
  for c in pkg_caches:
    args.add(["-package-db", c.dirname])

  # Make all external dependency files available.
  external_files = depset([f for dep in ctx.attr.external_deps
                             for f in dep.files])
  for include_dir in depset([f.dirname for f in external_files]):
    args.add("-I{0}".format(include_dir))

  args.add(ctx.files.c_sources)

  output_files = [ctx.actions.declare_file(path_append(output_dir.basename, replace_ext(s.path, output_ext)))
                        for s in ctx.files.c_sources]
  ctx.actions.run(
    inputs = ctx.files.c_sources + (external_files + pkg_caches).to_list(),
    outputs = [output_dir] + output_files,
    use_default_shell_env = True,
    progress_message = "Compiling C dynamic {0}".format(ctx.attr.name),
    executable = "ghc",
    arguments = [user_args, args],
  )
  return output_files
