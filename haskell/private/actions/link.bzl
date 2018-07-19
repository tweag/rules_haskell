"""Actions for linking object code produced by compilation"""

load(":private/pkg_id.bzl", "pkg_id")
load(":private/set.bzl", "set")
load(":private/path_utils.bzl", "get_lib_name")
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

def _fix_linker_paths(hs, inp, out, external_libraries):
    """Postprocess a macOS binary to make shared library references relative.

    On macOS, in order to simulate the linker "rpath" behavior and make the
    binary load shared libraries from relative paths, (or dynamic libraries
    load other libraries) we need to postprocess it with install_name_tool.
    (This is what the Bazel-provided `cc_wrapper.sh` does for cc rules.)
    For details: https://blogs.oracle.com/dipol/entry/dynamic_libraries_rpath_and_mac

    Args:
      hs: Haskell context.
      inp: An input file.
      out: An output file.
      external_libraries: A list of C library dependencies to make relative.
    """
    hs.actions.run_shell(
        inputs = [inp],
        outputs = [out],
        mnemonic = "HaskellFixupLoaderPath",
        progress_message = "Fixing install paths for {0}".format(out.basename),
        command = " &&\n    ".join(
            [
                "cp {} {}".format(inp.path, out.path),
                "chmod +w {}".format(out.path),
            ] +
            [
                "/usr/bin/install_name_tool -change {} {} {}".format(
                    f.path,
                    paths.join("@loader_path", _backup_path(out), f.path),
                    out.path,
                )
                for f in external_libraries
            ],
        ),
    )

def link_binary(
        hs,
        cc,
        dep_info,
        extra_srcs,
        compiler_flags,
        object_files,
        with_profiling,
        dummy_static_lib):
    """Link Haskell binary from static object files.

    Returns:
      File: produced executable
    """

    executable = hs.actions.declare_file(hs.name)
    if not hs.toolchain.is_darwin:
        compile_output = executable
    else:
        compile_output = hs.actions.declare_file(hs.name + ".temp")
        _fix_linker_paths(
            hs,
            compile_output,
            executable,
            dep_info.external_libraries,
        )

    args = hs.actions.args()
    args.add(["-optl" + f for f in cc.linker_flags])
    if with_profiling:
        args.add("-prof")
    args.add(hs.toolchain.compiler_flags)
    args.add(compiler_flags)

    if hs.toolchain.is_darwin:
        args.add(["-optl-Wl,-headerpad_max_install_names"])

        # Nixpkgs commit 3513034208a introduces -liconv in NIX_LDFLAGS on
        # Darwin. We don't currently handle NIX_LDFLAGS in any special
        # way, so a hack is to simply do what NIX_LDFLAGS is telling us we
        # should do always when using a toolchain from Nixpkgs.
        # TODO remove this gross hack.
        # TODO: enable dynamic linking of Haskell dependencies for macOS.
        args.add("-liconv")
    elif not with_profiling:
        args.add(["-pie", "-dynamic"])

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

    if hs.toolchain.is_darwin:
        # Suppress a warning that Clang prints due to GHC automatically passing
        # "-pie" or "-no-pie" to the C compiler.
        # This particular invocation of GHC is a little unusual; e.g., we're
        # passing an empty archive so that GHC has some input files to work on
        # during linking.
        args.add([
            "-optc-Wno-unused-command-line-argument",
            "-optl-Wno-unused-command-line-argument",
        ])
    else:
        for rpath in set.to_list(_infer_rpaths(executable, solibs)):
            args.add(["-optl-Wl,-rpath," + rpath])

    hs.toolchain.actions.run_ghc(
        hs,
        inputs = depset(transitive = [
            depset(extra_srcs),
            set.to_depset(dep_info.package_caches),
            set.to_depset(dep_info.dynamic_libraries),
            depset(dep_info.static_libraries),
            depset(dep_info.static_libraries_prof),
            depset(object_files),
            depset([dummy_static_lib]),
            depset(dep_info.external_libraries.values()),
        ]),
        outputs = [compile_output],
        mnemonic = "HaskellLinkBinary",
        arguments = [args],
    )

    return executable

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
            ),
        )
        set.mutable_insert(r, "$ORIGIN/" + rpath)

    return r

def _so_extension(hs):
    """Returns the extension for shared libraries.

    Args:
      ctx: Rule context.

    Returns:
      string of extension.
    """
    return "dylib" if hs.toolchain.is_darwin else "so"

def link_library_static(hs, cc, dep_info, object_files, my_pkg_id, with_profiling):
    """Link a static library for the package using given object files.

    Returns:
      File: Produced static library.
    """
    static_library = hs.actions.declare_file(
        "lib{0}.a".format(pkg_id.library_name(hs, my_pkg_id, prof_suffix = with_profiling)),
    )

    args = hs.actions.args()
    args.add(["qc", static_library])
    args.add(object_files)

    hs.actions.run(
        inputs = object_files + hs.tools_runfiles.ar,
        outputs = [static_library],
        mnemonic = "HaskellLinkStaticLibrary",
        executable = hs.tools.ar,
        arguments = [args],
    )
    return static_library

def link_library_dynamic(hs, cc, dep_info, extra_srcs, object_files, my_pkg_id):
    """Link a dynamic library for the package using given object files.

    Returns:
      File: Produced dynamic library.
    """
    dynamic_library = hs.actions.declare_file(
        "lib{0}-ghc{1}.{2}".format(
            pkg_id.library_name(hs, my_pkg_id),
            hs.toolchain.version,
            _so_extension(hs),
        ),
    )

    args = hs.actions.args()
    args.add(["-optl" + f for f in cc.linker_flags])

    args.add(["-shared", "-dynamic"])

    # Work around macOS linker limits.  This fix has landed in GHC HEAD, but is
    # not yet in a release; plus, we still want to support older versions of
    # GHC.  For details, see: https://phabricator.haskell.org/D4714
    if hs.toolchain.is_darwin:
        args.add(["-optl-Wl,-dead_strip_dylibs"])

    for package in set.to_list(dep_info.package_ids):
        args.add(["-package-id", package])

    # XXX This should be really dep_info.direct_prebuilt_deps, but since we
    # cannot add prebuilt_dependencies to the "depends" field on package
    # registration (see a comment there), we have to pass all transitive
    # prebuilt_dependencies on linking like this.
    for package in set.to_list(dep_info.prebuilt_dependencies):
        args.add(["-package", package])

    for cache in set.to_list(dep_info.package_caches):
        args.add(["-package-db", cache.dirname])

    _add_external_libraries(args, dep_info.external_libraries.values())

    args.add([f.path for f in object_files])

    solibs = set.union(
        set.from_list(dep_info.external_libraries),
        dep_info.dynamic_libraries,
    )

    if hs.toolchain.is_darwin:
        dynamic_library_tmp = hs.actions.declare_file(dynamic_library.basename + ".temp")
        _fix_linker_paths(
            hs,
            dynamic_library_tmp,
            dynamic_library,
            dep_info.external_libraries,
        )
        args.add(["-optl-Wl,-headerpad_max_install_names"])
    else:
        dynamic_library_tmp = dynamic_library
        for rpath in set.to_list(_infer_rpaths(dynamic_library, solibs)):
            args.add(["-optl-Wl,-rpath," + rpath])

    args.add(["-o", dynamic_library_tmp.path])

    hs.toolchain.actions.run_ghc(
        hs,
        inputs = depset(transitive = [
            depset(extra_srcs),
            depset(object_files),
            set.to_depset(dep_info.package_caches),
            set.to_depset(dep_info.dynamic_libraries),
            depset(dep_info.external_libraries.values()),
        ]),
        outputs = [dynamic_library_tmp],
        mnemonic = "HaskellLinkDynamicLibrary",
        arguments = [args],
    )

    return dynamic_library
