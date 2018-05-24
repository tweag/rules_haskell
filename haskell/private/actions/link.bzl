"""Actions for linking object code produced by compilation"""

load(":private/actions/package.bzl", "get_library_name")
load(":private/mode.bzl", "add_mode_options")
load(":private/set.bzl", "set")
load(":private/tools.bzl",
     "get_ghc_version",
     "is_darwin",
     "so_extension",
     "tools",
     "tools_runfiles",
)
load(":private/utils.bzl", "get_lib_name")
load("@bazel_skylib//:lib.bzl", "paths")

def _backup_path(target):
  """Return a path from the directory this is in to the Bazel root.

  Args:
    target: File

  Returns:
    A path of the form "../../.."
  """
  n = len(target.dirname.split("/"))

  return "/".join([".."] * n)

def _create_dummy_archive(ctx):
  """Create empty archive so that GHC has some input files to work on during
  linking.

  See: https://github.com/facebook/buck/blob/126d576d5c07ce382e447533b57794ae1a358cc2/src/com/facebook/buck/haskell/HaskellDescriptionUtils.java#L295

  Args:
    ctx: Rule context.

  Returns:
    File, the created dummy archive.
  """

  dummy_raw = "BazelDummy.hs"
  dummy_input = ctx.actions.declare_file(dummy_raw)
  dummy_object = ctx.actions.declare_file(paths.replace_extension(dummy_raw, ".o"))

  ctx.actions.write(output=dummy_input, content="""
{-# LANGUAGE NoImplicitPrelude #-}
module BazelDummy () where
""")

  dummy_static_lib = ctx.actions.declare_file("libempty.a")
  ctx.actions.run(
    inputs = [dummy_input],
    outputs = [dummy_object],
    executable = tools(ctx).ghc,
    arguments = ["-c", dummy_input.path],
  )

  ar_args = ctx.actions.args()
  ar_args.add(["qc", dummy_static_lib, dummy_object])

  ctx.actions.run(
    inputs = [dummy_object] + tools_runfiles(ctx).ar,
    outputs = [dummy_static_lib],
    executable = tools(ctx).ar,
    arguments = [ar_args]
  )

  return dummy_static_lib

def _fix_linker_paths(ctx, inp, out, external_libraries):
  """Postprocess a macOS binary to make shared library references relative.

  On macOS, in order to simulate the linker "rpath" behavior and make the
  binary load shared libraries from relative paths, (or dynamic libraries
  load other libraries) we need to postprocess it with install_name_tool.
  (This is what the Bazel-provided `cc_wrapper.sh` does for cc rules.)
  For details: https://blogs.oracle.com/dipol/entry/dynamic_libraries_rpath_and_mac

  Args:
    ctx: Rule context.
    inp: An input file.
    out: An output file.
    external_libraries: A list of C library dependencies to make relative.
  """
  ctx.actions.run_shell(
      inputs=[inp],
      outputs=[out],
      progress_message =
          "Fixing install paths for {0}".format(out.basename),
      command = " &&\n    ".join(
          ["cp {} {}".format(inp.path, out.path),
           "chmod +w {}".format(out.path)]
          + ["/usr/bin/install_name_tool -change {} {} {}"
             .format(f.path,
                     paths.join("@loader_path", _backup_path(out), f.path),
                     out.path)
                     for f in external_libraries]))

def link_haskell_bin(ctx, dep_info, object_files):
  """Link Haskell binary from static object files.

  Args:
    ctx: Rule context.
    object_files: Dynamic object files.

  Returns:
    File: produced executable
  """

  dummy_static_lib = _create_dummy_archive(ctx)

  if not is_darwin(ctx):
    compile_output = ctx.outputs.executable
  else:
    compile_output = ctx.actions.declare_file(ctx.outputs.executable.basename + ".temp")
    _fix_linker_paths(ctx, compile_output, ctx.outputs.executable,
                      dep_info.external_libraries)

  args = ctx.actions.args()
  add_mode_options(ctx, args)
  args.add(ctx.attr.compiler_flags)

  if is_darwin(ctx):
    args.add(["-optl-Wl,-headerpad_max_install_names"])
  else:
    # TODO: enable dynamic linking of Haskell dependencies for macOS.
    args.add(["-dynamic", "-pie"])

  args.add(["-o", compile_output.path, dummy_static_lib.path])

  # De-duplicate optl calls while preserving ordering: we want last
  # invocation of an object to remain last. That is `-optl foo -optl
  # bar -optl foo` becomes `-optl bar -optl foo`. Do this by counting
  # number of occurrences. That way we only build dict and add to args
  # directly rather than doing multiple reversals with temporary
  # lists.

  args.add([f.path for f in object_files])

  for package in set.to_list(dep_info.package_ids):
    args.add(["-package-id", package])

  for cache in set.to_list(dep_info.package_caches):
    args.add(["-package-db", cache.dirname])

  # We have to remember to specify all (transitive) wired-in
  # dependencies or we can't find objects for linking.
  for p in set.to_list(dep_info.prebuilt_dependencies):
    args.add(["-package", p])

  _add_external_libraries(args, dep_info.external_libraries.values())

  solibs = set.union(
    set.from_list(dep_info.external_libraries),
    dep_info.dynamic_libraries,
  )

  if is_darwin(ctx):
    # Suppress a warning that Clang prints due to GHC automatically passing
    # "-pie" or "-no-pie" to the C compiler.
    # This particular invocation of GHC is a little unusual; e.g., we're
    # passing an empty archive so that GHC has some input files to work on
    # during linking.
    args.add(["-optc-Wno-unused-command-line-argument",
              "-optl-Wno-unused-command-line-argument"])
  else:
    for rpath in set.to_list(_infer_rpaths(ctx.outputs.executable, solibs)):
      args.add(["-optl-Wl,-rpath," + rpath])

  ctx.actions.run(
    inputs = depset(transitive = [
      set.to_depset(dep_info.package_caches),
      set.to_depset(dep_info.dynamic_libraries),
      depset(dep_info.static_libraries),
      depset(object_files),
      depset([dummy_static_lib]),
      depset(dep_info.external_libraries.values()),
    ]),
    outputs = [compile_output],
    progress_message = "Linking {0}".format(ctx.attr.name),
    executable = tools(ctx).ghc,
    arguments = [args]
  )

  return ctx.outputs.executable

def _add_external_libraries(args, libs):
  """Add options to `args` that allow us to link to `libs`.

  Args:
    args: Args object.
    libs: list of external shared libraries.
  """
  seen_libs = set.empty()
  for lib in libs:
    lib_name = get_lib_name(lib)
    if not set.is_member(seen_libs, lib_name):
      set.mutable_insert(seen_libs, lib_name)
      args.add([
        "-l{0}".format(lib_name),
        "-L{0}".format(paths.dirname(lib.path)),
      ])

def _infer_rpaths(target, solibs):
  """Return set of RPATH values to be added to target so it can find all
  solibs.

  Args:
    target: File, executable or library we're linking.
    solibs: A set of Files, shared objects that the target needs.

  Returns:
    Set of strings: rpaths to add to target.
  """
  r = set.empty()

  for solib in set.to_list(solibs):
    rpath = paths.normalize(
      paths.join(
        _backup_path(target),
        solib.dirname,
      )
    )
    set.mutable_insert(r, "$ORIGIN/" + rpath)

  return r

def link_static_lib(ctx, dep_info, object_files):
  """Link a static library for the package using given object files.

  Args:
    ctx: Rule context.
    object_files: All object files to include in the library.

  Returns:
    File: Produced static library.
  """
  static_library = ctx.actions.declare_file("lib{0}.a".format(get_library_name(ctx)))

  args = ctx.actions.args()
  args.add(["qc", static_library])
  args.add(object_files)

  ctx.actions.run(
    inputs = object_files + tools_runfiles(ctx).ar,
    outputs = [static_library],
    progress_message = "Linking static library {0}".format(static_library.basename),
    executable = tools(ctx).ar,
    arguments = [args],
  )
  return static_library

def link_dynamic_lib(ctx, dep_info, object_files):
  """Link a dynamic library for the package using given object files.

  Args:
    ctx: Rule context.
    object_files: Object files to use for linking.

  Returns:
    File: Produced dynamic library.
  """

  version = get_ghc_version(ctx)
  dynamic_library = ctx.actions.declare_file(
    "lib{0}-ghc{1}.{2}".format(get_library_name(ctx), version, so_extension(ctx))
  )

  args = ctx.actions.args()

  add_mode_options(ctx, args)

  args.add(["-shared", "-dynamic"])

  for package in set.to_list(dep_info.package_ids):
    args.add(["-package-id", package])
  for package in set.to_list(dep_info.prebuilt_dependencies):
    args.add(["-package", package])

  for cache in set.to_list(dep_info.package_caches):
    args.add(["-package-db", cache.dirname])

  _add_external_libraries(args, dep_info.external_libraries.values())

  args.add([ f.path for f in object_files ])

  solibs = set.union(
    set.from_list(dep_info.external_libraries),
    dep_info.dynamic_libraries,
  )

  if is_darwin(ctx):
    dynamic_library_tmp = ctx.actions.declare_file(dynamic_library.basename + ".temp")
    _fix_linker_paths(ctx, dynamic_library_tmp, dynamic_library,
                      dep_info.external_libraries)
    args.add(["-optl-Wl,-headerpad_max_install_names"])
  else:
    dynamic_library_tmp = dynamic_library
    for rpath in set.to_list(_infer_rpaths(dynamic_library, solibs)):
      args.add(["-optl-Wl,-rpath," + rpath])

  args.add(["-o", dynamic_library_tmp.path])

  ctx.actions.run(
    inputs = depset(transitive = [
      depset(object_files),
      set.to_depset(dep_info.package_caches),
      set.to_depset(dep_info.dynamic_libraries),
      depset(dep_info.external_libraries.values()),
    ]),
    outputs = [dynamic_library_tmp],
    progress_message = "Linking dynamic library {0}".format(dynamic_library.basename),
    executable = tools(ctx).ghc,
    arguments = [args]
  )

  return dynamic_library
