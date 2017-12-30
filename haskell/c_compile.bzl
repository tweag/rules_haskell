"""C file compilation."""

load(":actions.bzl",
  "HaskellPackageInfo",
  "mk_name",
)

load(":tools.bzl", "get_compiler")

load("@bazel_skylib//:lib.bzl", "paths")

def c_compile_static(ctx):
  """Compile all C files to static object files.

  TODO: We would like to compile these one at a time. This is somewhat
  difficult as they all go into the same object directory and
  therefore share prefix. Possibly we don't care about where in the
  hierarchy they go though so we may be able to just put them in
  separate directory each. Or something.

  Args:
    ctx: Rule context.

  Returns:
    list of File: Compiled static object files.
  """
  return _generic_c_compile(ctx, "objects_c", ".o", [])

def c_compile_dynamic(ctx):
  """Compile all C files to dynamic object files.

  TODO: We would like to compile these one at a time. This is somewhat
  difficult as they all go into the same object directory and
  therefore share prefix. Possibly we don't care about where in the
  hierarchy they go though so we may be able to just put them in
  separate directory each. Or something.

  Args:
    ctx: Rule context.

  Returns:
    list of File: Compiled dynamic object files.
  """
  return _generic_c_compile(ctx, "objects_c_dyn", ".dyn_o", ["-dynamic"])

def _generic_c_compile(ctx, output_dir_template, output_ext, user_args):
  """Compile some C files in specified way.

  Args:
    ctx: Rule context.
    output_dir_template: Template for object file output directory.
    output_ext: Expected object file extension that compiled files will have.
    user_args: List of user-provided arguments driving compilation mode.

  Returns:
    list of File: Compiled object files.
  """
  # Directory for objects generated from C files.
  output_dir = ctx.actions.declare_directory(mk_name(ctx, output_dir_template))
  args = ctx.actions.args()
  args.add([
    "-c",
    "-fPIC",
    "-odir", output_dir,
    "-osuf", output_ext,
  ])
  args.add(user_args)

  for opt in ctx.attr.c_options:
    args.add("-optc{0}".format(opt))

  # TODO: use gather_dep_info instead, we don't need this. Even
  # better, factor out -package logic to share it with
  # compilation_defaults.
  pkg_caches = depset()
  pkg_names = depset()
  for d in ctx.attr.deps:
    if HaskellPackageInfo in d:
      pkg_caches = depset(transitive = [pkg_caches, d[HaskellPackageInfo].caches])
      pkg_names = depset(transitive = [pkg_names, d[HaskellPackageInfo].names])

  # Expose every dependency and every prebuilt dependency.
  for n in depset(transitive = [pkg_names, depset(ctx.attr.prebuilt_dependencies)]).to_list():
    args.add(["-package", n])

  # Point at every package DB we depend on and know of explicitly.
  for c in pkg_caches.to_list():
    args.add(["-package-db", c.dirname])

  # Make all external dependency files available.
  external_files = depset([f for dep in ctx.attr.external_deps
                             for f in dep.files])
  for include_dir in depset([f.dirname for f in external_files.to_list()]).to_list():
    args.add("-I{0}".format(include_dir))

  args.add(ctx.files.c_sources)

  output_files = [ctx.actions.declare_file(paths.join(output_dir.basename, paths.replace_extension(s.path, output_ext)))
                        for s in ctx.files.c_sources]
  ctx.actions.run(
    inputs = depset(transitive = [
      depset(ctx.files.c_sources),
      external_files,
      pkg_caches,
    ]),
    outputs = [output_dir] + output_files,
    progress_message = "Compiling C {0}{1}".format(
      ctx.attr.name,
      # Show user-provided flags to user. This way we can tell what
      # we're actually doing.
      " ({0})".format(" ".join(user_args)) if user_args != [] else "",
    ),
    executable = get_compiler(ctx),
    arguments = [args],
  )
  return output_files
