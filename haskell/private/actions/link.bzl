"""Actions for linking object code produced by compilation"""

load(":private/packages.bzl", "expose_packages")
load("@bazel_skylib//lib:paths.bzl", "paths")
load(":private/path_utils.bzl", "get_lib_name")
load(":private/pkg_id.bzl", "pkg_id")
load(":private/set.bzl", "set")
load(":private/providers.bzl", "external_libraries_get_mangled")

def _backup_path(target):
    """Return a path from the directory this is in to the Bazel root.

    Args:
      target: File

    Returns:
      A path of the form "../../.."
    """
    n = len(target.dirname.split("/"))

    return "/".join([".."] * n)

def _fix_darwin_linker_paths(hs, inp, out, external_libraries):
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
      external_libraries: HaskellBuildInfo external_libraries to make relative.
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
                # Patch the "install name" or "library identifaction name".
                # The "install name" informs targets that link against `out`
                # where `out` can be found during runtime. Here we update this
                # "install name" to the new filename of the fixed binary.
                # Refer to the Oracle blog post linked above for details.
                "/usr/bin/install_name_tool -id @rpath/{} {}".format(
                    out.basename,
                    out.path,
                ),
            ] +
            [
                # Make rpaths for external library dependencies relative to the
                # binary's installation path, rather than the working directory
                # at execution time.
                "/usr/bin/install_name_tool -change {} {} {}".format(
                    f.lib.path,
                    paths.join("@loader_path", _backup_path(out), f.lib.path),
                    out.path,
                )
                # we use the unmangled lib (f.lib) for this instead of a mangled lib name
                for f in set.to_list(external_libraries)
            ],
        ),
    )

def _create_objects_dir_manifest(hs, objects_dir, dynamic, with_profiling):
    suffix = ".dynamic.manifest" if dynamic else ".static.manifest"
    objects_dir_manifest = hs.actions.declare_file(
        objects_dir.basename + suffix,
        sibling = objects_dir,
    )

    if with_profiling:
        ext = "p_o"
    elif dynamic:
        ext = "dyn_o"
    else:
        ext = "o"
    hs.actions.run_shell(
        inputs = [objects_dir],
        outputs = [objects_dir_manifest],
        command = """
        find {dir} -name '*.{ext}' > {out}
        """.format(
            dir = objects_dir.path,
            ext = ext,
            out = objects_dir_manifest.path,
        ),
        use_default_shell_env = True,
    )

    return objects_dir_manifest

def link_binary(
        hs,
        cc,
        dep_info,
        extra_srcs,
        compiler_flags,
        objects_dir,
        dynamic,
        with_profiling,
        version):
    """Link Haskell binary from static object files.

    Returns:
      File: produced executable
    """

    executable = hs.actions.declare_file(hs.name)
    if not hs.toolchain.is_darwin:
        compile_output = executable
    else:
        compile_output = hs.actions.declare_file(hs.name + ".temp")
        _fix_darwin_linker_paths(
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

    # By default, GHC will produce mostly-static binaries, i.e. in which all
    # Haskell code is statically linked and foreign libraries and system
    # dependencies are dynamically linked. If linkstatic is false, i.e. the user
    # has requested fully dynamic linking, we must therefore add flags to make
    # sure that GHC dynamically links Haskell code too. The one exception to
    # this is when we are compiling for profiling, which currently does not play
    # nicely with dynamic linking.
    if dynamic:
        if with_profiling:
            print("WARNING: dynamic linking and profiling don't mix. Omitting -dynamic.\nSee https://ghc.haskell.org/trac/ghc/ticket/15394")
        else:
            args.add(["-pie", "-dynamic"])

    # When compiling with `-threaded`, GHC needs to link against
    # the pthread library when linking against static archives (.a).
    # We assume it’s not a problem to pass it for other cases,
    # so we just default to passing it.
    args.add("-optl-pthread")

    args.add(["-o", compile_output.path])

    # De-duplicate optl calls while preserving ordering: we want last
    # invocation of an object to remain last. That is `-optl foo -optl
    # bar -optl foo` becomes `-optl bar -optl foo`. Do this by counting
    # number of occurrences. That way we only build dict and add to args
    # directly rather than doing multiple reversals with temporary
    # lists.

    args.add(expose_packages(
        dep_info,
        lib_info = None,
        use_direct = False,
        use_my_pkg_id = None,
        custom_package_caches = None,
        version = version,
    ))

    _add_external_libraries(args, dep_info.external_libraries)

    solibs = set.union(
        set.map(dep_info.external_libraries, external_libraries_get_mangled),
        dep_info.dynamic_libraries,
    )

    # XXX: Suppress a warning that Clang prints due to GHC automatically passing
    # "-pie" or "-no-pie" to the C compiler.
    # This is linked to https://ghc.haskell.org/trac/ghc/ticket/15319
    args.add([
        "-optc-Wno-unused-command-line-argument",
        "-optl-Wno-unused-command-line-argument",
    ])

    if hs.toolchain.is_darwin:
        args.add(["-optl-Wl,-headerpad_max_install_names"])

        # Nixpkgs commit 3513034208a introduces -liconv in NIX_LDFLAGS on
        # Darwin. We don't currently handle NIX_LDFLAGS in any special
        # way, so a hack is to simply do what NIX_LDFLAGS is telling us we
        # should do always when using a toolchain from Nixpkgs.
        # TODO remove this gross hack.
        args.add("-liconv")

    for rpath in set.to_list(_infer_rpaths(hs.toolchain.is_darwin, executable, solibs)):
        args.add(["-optl-Wl,-rpath," + rpath])

    objects_dir_manifest = _create_objects_dir_manifest(
        hs,
        objects_dir,
        dynamic = dynamic,
        with_profiling = with_profiling,
    )
    hs.toolchain.actions.run_ghc(
        hs,
        inputs = depset(transitive = [
            depset(extra_srcs),
            set.to_depset(dep_info.package_caches),
            set.to_depset(dep_info.dynamic_libraries),
            depset(dep_info.static_libraries),
            depset(dep_info.static_libraries_prof),
            depset([objects_dir]),
            depset([e.mangled_lib for e in set.to_list(dep_info.external_libraries)]),
            depset(hs.extra_binaries),
        ]),
        outputs = [compile_output],
        mnemonic = "HaskellLinkBinary",
        arguments = [args],
        params_file = objects_dir_manifest,
    )

    return executable

def _add_external_libraries(args, ext_libs):
    """Add options to `args` that allow us to link to `ext_libs`.

    Args:
      args: Args object.
      ext_libs: external_libraries from HaskellBuildInfo
    """
    seen_libs = set.empty()
    for ext_lib in set.to_list(ext_libs):
        lib = ext_lib.mangled_lib
        lib_name = get_lib_name(lib)
        if not set.is_member(seen_libs, lib_name):
            set.mutable_insert(seen_libs, lib_name)
            args.add([
                "-l{0}".format(lib_name),
                "-L{0}".format(paths.dirname(lib.path)),
            ])

def _infer_rpaths(is_darwin, target, solibs):
    """Return set of RPATH values to be added to target so it can find all
    solibs.

    Args:
      is_darwin: Whether we're compiling on and for Darwin.
      target: File, executable or library we're linking.
      solibs: A set of Files, shared objects that the target needs.

    Returns:
      Set of strings: rpaths to add to target.
    """
    r = set.empty()

    if is_darwin:
        origin = "@loader_path/"
    else:
        origin = "$ORIGIN/"

    for solib in set.to_list(solibs):
        rpath = paths.normalize(
            paths.join(
                _backup_path(target),
                solib.dirname,
            ),
        )
        set.mutable_insert(r, origin + rpath)

    return r

def _so_extension(hs):
    """Returns the extension for shared libraries.

    Args:
      ctx: Rule context.

    Returns:
      string of extension.
    """
    return "dylib" if hs.toolchain.is_darwin else "so"

def link_library_static(hs, cc, dep_info, objects_dir, my_pkg_id, with_profiling):
    """Link a static library for the package using given object files.

    Returns:
      File: Produced static library.
    """
    static_library = hs.actions.declare_file(
        "lib{0}.a".format(pkg_id.library_name(hs, my_pkg_id, prof_suffix = with_profiling)),
    )
    objects_dir_manifest = _create_objects_dir_manifest(
        hs,
        objects_dir,
        dynamic = False,
        with_profiling = with_profiling,
    )
    args = hs.actions.args()
    inputs = ([objects_dir, objects_dir_manifest, hs.tools.ar] +
              hs.tools_runfiles.ar + hs.extra_binaries)

    if hs.toolchain.is_darwin:
        # On Darwin, ar doesn't support params files.
        args.add([
            static_library,
            objects_dir_manifest.path,
        ])

        # TODO Get ar location from the CC toolchain. This is
        # complicated by the fact that the CC toolchain does not
        # always use ar, and libtool has an entirely different CLI.
        # See https://github.com/bazelbuild/bazel/issues/5127
        hs.actions.run_shell(
            inputs = inputs,
            outputs = [static_library],
            mnemonic = "HaskellLinkStaticLibrary",
            command = "{ar} qc $1 $(< $2)".format(ar = hs.tools.ar.path),
            arguments = [args],

            # Use the default macosx toolchain
            env = {"SDKROOT": "macosx"},
        )
    else:
        args.add([
            "qc",
            static_library,
            "@" + objects_dir_manifest.path,
        ])
        hs.actions.run(
            inputs = inputs,
            outputs = [static_library],
            mnemonic = "HaskellLinkStaticLibrary",
            executable = hs.tools.ar,
            arguments = [args],
        )

    return static_library

def link_library_dynamic(hs, cc, dep_info, extra_srcs, objects_dir, my_pkg_id):
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

    args.add(expose_packages(
        dep_info,
        lib_info = None,
        use_direct = False,
        use_my_pkg_id = None,
        custom_package_caches = None,
        version = my_pkg_id.version if my_pkg_id else None,
    ))

    _add_external_libraries(args, dep_info.external_libraries)

    solibs = set.union(
        set.map(dep_info.external_libraries, external_libraries_get_mangled),
        dep_info.dynamic_libraries,
    )

    if hs.toolchain.is_darwin:
        dynamic_library_tmp = hs.actions.declare_file(dynamic_library.basename + ".temp")
        _fix_darwin_linker_paths(
            hs,
            dynamic_library_tmp,
            dynamic_library,
            dep_info.external_libraries,
        )
        args.add(["-optl-Wl,-headerpad_max_install_names"])
    else:
        dynamic_library_tmp = dynamic_library

    for rpath in set.to_list(_infer_rpaths(hs.toolchain.is_darwin, dynamic_library, solibs)):
        args.add(["-optl-Wl,-rpath," + rpath])

    args.add(["-o", dynamic_library_tmp.path])

    # Profiling not supported for dynamic libraries.
    objects_dir_manifest = _create_objects_dir_manifest(
        hs,
        objects_dir,
        dynamic = True,
        with_profiling = False,
    )

    hs.toolchain.actions.run_ghc(
        hs,
        inputs = depset([objects_dir], transitive = [
            depset(hs.extra_binaries),
            depset(extra_srcs),
            set.to_depset(dep_info.package_caches),
            set.to_depset(dep_info.dynamic_libraries),
            depset([e.mangled_lib for e in set.to_list(dep_info.external_libraries)]),
        ]),
        outputs = [dynamic_library_tmp],
        mnemonic = "HaskellLinkDynamicLibrary",
        arguments = [args],
        params_file = objects_dir_manifest,
    )

    return dynamic_library
