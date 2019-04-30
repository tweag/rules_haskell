"""Actions for linking object code produced by compilation"""

load(":private/packages.bzl", "expose_packages", "pkg_info_to_compile_flags")
load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    ":private/path_utils.bzl",
    "get_lib_name",
    "is_shared_library",
    "is_static_library",
    "ln",
)
load(":private/pkg_id.bzl", "pkg_id")
load(":private/set.bzl", "set")
load(":private/list.bzl", "list")

# tests in /tests/unit_tests/BUILD
def parent_dir_path(path):
    """Returns the path of the parent directory.
    For a relative path with just a file, "." is returned.
    The path is not normalized.

    foo => .
    foo/ => foo
    foo/bar => foo
    foo/bar/baz => foo/bar
    foo/../bar => foo/..

    Args:
      a path string

    Returns:
      A path list of the form `["foo", "bar"]`
    """
    path_dir = paths.dirname(path)

    # dirname returns "" if there is no parent directory
    # In that case we return the identity path, which is ".".
    if path_dir == "":
        return ["."]
    else:
        return path_dir.split("/")

def __check_dots(target, path):
    # there’s still (non-leading) .. in split
    if ".." in path:
        fail("the short_path of target {} (which is {}) contains more dots than loading `../`. We can’t handle that.".format(
            target,
            target.short_path,
        ))

# skylark doesn’t allow nested defs, which is a mystery.
def _get_target_parent_dir(target):
    """get the parent dir and handle leading short_path dots,
    which signify that the target is in an external repository.

    Args:
      target: a target, .short_path is used
    Returns:
      (is_external, parent_dir)
      `is_external`: Bool whether the path points to an external repository
      `parent_dir`: The parent directory, either up to the runfiles toplel,
                    up to the external repository toplevel.
                    Is `[]` if there is no parent dir.
    """

    parent_dir = parent_dir_path(target.short_path)

    if parent_dir[0] == "..":
        __check_dots(target, parent_dir[1:])
        return (True, parent_dir[1:])
    elif parent_dir[0] == ".":
        return (False, [])
    else:
        __check_dots(target, parent_dir)
        return (False, parent_dir)

# tests in /tests/unit_tests/BUILD
def create_rpath_entry(
        binary,
        dependency,
        keep_filename,
        prefix = ""):
    """Return a (relative) path that points from `binary` to `dependecy`
    while not leaving the current bazel runpath, taking into account weird
    corner cases of `.short_path` concerning external repositories.
    The resulting entry should be able to be inserted into rpath or similar.

    Examples:

      bin.short_path=foo/a.so and dep.short_path=bar/b.so
        => create_rpath_entry(bin, dep, False) = ../bar
           and
           create_rpath_entry(bin, dep, True) = ../bar/b.so
           and
           create_rpath_entry(bin, dep, True, "$ORIGIN") = $ORIGIN/../bar/b.so

    Args:
      binary: target of current binary
      dependency: target of dependency to relatively point to
      keep_filename: whether to point to the filename or its parent dir
      prefix: string path prefix to add before the relative path

    Returns:
      relative path string
    """

    (bin_is_external, bin_parent_dir) = _get_target_parent_dir(binary)
    (dep_is_external, dep_parent_dir) = _get_target_parent_dir(dependency)

    # backup through parent directories of the binary,
    # to the runfiles directory
    bin_backup = [".."] * len(bin_parent_dir)

    # external repositories live in `target.runfiles/external`,
    # while the internal repository lives in `target.runfiles`.
    # The `.short_path`s of external repositories are strange,
    # they start with `../`, but you cannot just append that in
    # order to find the correct runpath. Instead you have to use
    # the following logic to construct the correct runpaths:
    if bin_is_external:
        if dep_is_external:
            # stay in `external`
            path_segments = bin_backup
        else:
            # backup out of `external`
            path_segments = [".."] + bin_backup
    elif dep_is_external:
        # go into `external`
        path_segments = bin_backup + ["external"]
    else:
        # no special external traversal
        path_segments = bin_backup

    # then add the parent dir to our dependency
    path_segments.extend(dep_parent_dir)

    # optionally add the filename
    if keep_filename:
        path_segments.append(
            paths.basename(dependency.short_path),
        )

    # normalize for good measure and create the final path
    path = paths.normalize("/".join(path_segments))

    # and add the prefix if applicable
    if prefix == "":
        return path
    else:
        return prefix + "/" + path

def _merge_parameter_files(hs, file1, file2):
    """Merge two GHC parameter files into one.

    Args:
      hs: Haskell context.
      file1: The first parameter file.
      file2: The second parameter file.

    Returns:
      File: A new parameter file containing the parameters of both input files.
        The file name is based on the file names of the input files. The file
        is located next to the first input file.
    """
    params_file = hs.actions.declare_file(
        file1.basename + ".and." + file2.basename,
        sibling = file1,
    )
    hs.actions.run_shell(
        inputs = [file1, file2],
        outputs = [params_file],
        command = """
            cat {file1} {file2} > {out}
        """.format(
            file1 = file1.path,
            file2 = file2.path,
            out = params_file.path,
        ),
    )
    return params_file

def _darwin_create_extra_linker_flags_file(hs, cc, objects_dir, executable, dynamic, solibs):
    """Write additional linker flags required on MacOS to a parameter file.

    Args:
      hs: Haskell context.
      cc: CcInteropInfo, information about C dependencies.
      objects_dir: Directory storing object files.
        Used to determine output file location.
      executable: The executable being built.
      dynamic: Bool: Whether to link dynamically or statically.
      solibs: List of dynamic library dependencies.

    Returns:
      File: Parameter file with additional linker flags. To be passed to GHC.
    """

    # On Darwin GHC will pass the dead_strip_dylibs flag to the linker. This
    # flag will remove any shared library loads from the binary's header that
    # are not directly resolving undefined symbols in the binary. I.e. any
    # indirect shared library dependencies will be removed. This conflicts with
    # Bazel's builtin cc rules, which assume that the final binary will load
    # all transitive shared library dependencies. In particlar shared libraries
    # produced by Bazel's cc rules never load shared libraries themselves. This
    # causes missing symbols at runtime on MacOS, see #170.
    #
    # The following work-around applies the `-u` flag to the linker for any
    # symbol that is undefined in any transitive shared library dependency.
    # This forces the linker to resolve these undefined symbols in all
    # transitive shared library dependencies and keep the corresponding load
    # commands in the binary's header.
    #
    # Unfortunately, this prohibits elimination of any truly redundant shared
    # library dependencies. Furthermore, the transitive closure of shared
    # library dependencies can be large, so this makes it more likely to exceed
    # the MACH-O header size limit on MacOS.
    #
    # This is a horrendous hack, but it seems to be forced on us by how Bazel
    # builds dynamic cc libraries.
    suffix = ".dynamic.linker_flags" if dynamic else ".static.linker_flags"
    linker_flags_file = hs.actions.declare_file(
        executable.basename + suffix,
        sibling = objects_dir,
    )

    hs.actions.run_shell(
        inputs = solibs,
        outputs = [linker_flags_file],
        command = """
        touch {out}
        for lib in {solibs}; do
            {nm} -u "$lib" | sed 's/^/-optl-Wl,-u,/' >> {out}
        done
        """.format(
            nm = cc.tools.nm,
            solibs = " ".join(["\"" + l.path + "\"" for l in solibs]),
            out = linker_flags_file.path,
        ),
    )
    return linker_flags_file

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

def _link_dependencies(hs, dep_info, dynamic, binary, args):
    """Configure linker flags and inputs.

    Configure linker flags for C library dependencies and runtime dynamic
    library dependencies. And collect the C libraries to pass as inputs to
    the linking action.

    Args:
      hs: Haskell context.
      dep_info: HaskellInfo provider.
      dynamic: Bool: Whether to link dynamically, or statically.
      binary: Final linked binary.
      args: Arguments to the linking action.

    Returns:
      depset: C library dependencies to provide as input to the linking action.
    """

    # Pick linking context based on linking mode.
    if dynamic:
        link_ctx = dep_info.cc_dependencies.dynamic_linking
        trans_link_ctx = dep_info.transitive_cc_dependencies.dynamic_linking
    else:
        link_ctx = dep_info.cc_dependencies.static_linking
        trans_link_ctx = dep_info.transitive_cc_dependencies.static_linking

    # Direct C library dependencies to link.
    # I.e. not indirect through another Haskell dependency.
    # Such indirect dependencies are linked by GHC based on the extra-libraries
    # fields in the dependency's package configuration file.
    libs_to_link = link_ctx.libraries_to_link.to_list()
    _add_external_libraries(args, libs_to_link)

    # Transitive library dependencies to have in scope for linking.
    trans_libs_to_link = trans_link_ctx.libraries_to_link.to_list()

    # Libraries to pass as inputs to linking action.
    cc_link_libs = depset(transitive = [
        depset(trans_libs_to_link),
    ])

    # Transitive dynamic library dependencies to have in RUNPATH.
    cc_solibs = trans_link_ctx.dynamic_libraries_for_runtime.to_list()

    # Collect Haskell dynamic library dependencies in common RUNPATH.
    # This is to keep the number of RUNPATH entries low, for faster loading
    # and to avoid exceeding the MACH-O header size limit on MacOS.
    hs_solibs = []
    if dynamic:
        hs_solibs_prefix = "_hssolib_%s" % hs.name
        for dep in set.to_list(dep_info.dynamic_libraries):
            dep_link = hs.actions.declare_file(
                paths.join(hs_solibs_prefix, dep.basename),
                sibling = binary,
            )
            ln(hs, dep, dep_link)
            hs_solibs.append(dep_link)

    # Configure RUNPATH.
    rpaths = _infer_rpaths(
        hs.toolchain.is_darwin,
        binary,
        trans_link_ctx.dynamic_libraries_for_runtime.to_list() +
        hs_solibs,
    )
    for rpath in set.to_list(rpaths):
        args.add("-optl-Wl,-rpath," + rpath)

    return (cc_link_libs, cc_solibs, hs_solibs)

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

    exe_name = hs.name + (".exe" if hs.toolchain.is_windows else "")
    executable = hs.actions.declare_file(exe_name)

    args = hs.actions.args()
    args.add_all(["-optl" + f for f in cc.linker_flags])
    if with_profiling:
        args.add("-prof")
    args.add_all(hs.toolchain.compiler_flags)
    args.add_all(compiler_flags)

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
            args.add_all(["-pie", "-dynamic"])

    # When compiling with `-threaded`, GHC needs to link against
    # the pthread library when linking against static archives (.a).
    # We assume it’s not a problem to pass it for other cases,
    # so we just default to passing it.
    args.add("-optl-pthread")

    args.add_all(["-o", executable.path])

    # De-duplicate optl calls while preserving ordering: we want last
    # invocation of an object to remain last. That is `-optl foo -optl
    # bar -optl foo` becomes `-optl bar -optl foo`. Do this by counting
    # number of occurrences. That way we only build dict and add to args
    # directly rather than doing multiple reversals with temporary
    # lists.

    args.add_all(pkg_info_to_compile_flags(expose_packages(
        dep_info,
        lib_info = None,
        use_direct = True,
        use_my_pkg_id = None,
        custom_package_databases = None,
        version = version,
    )))

    (cc_link_libs, cc_solibs, hs_solibs) = _link_dependencies(
        hs = hs,
        dep_info = dep_info,
        dynamic = dynamic,
        binary = executable,
        args = args,
    )

    # XXX: Suppress a warning that Clang prints due to GHC automatically passing
    # "-pie" or "-no-pie" to the C compiler.
    # This is linked to https://ghc.haskell.org/trac/ghc/ticket/15319
    args.add_all([
        "-optc-Wno-unused-command-line-argument",
        "-optl-Wno-unused-command-line-argument",
    ])

    objects_dir_manifest = _create_objects_dir_manifest(
        hs,
        objects_dir,
        dynamic = dynamic,
        with_profiling = with_profiling,
    )

    extra_linker_flags_file = None
    if hs.toolchain.is_darwin:
        args.add("-optl-Wl,-headerpad_max_install_names")

        # Nixpkgs commit 3513034208a introduces -liconv in NIX_LDFLAGS on
        # Darwin. We don't currently handle NIX_LDFLAGS in any special
        # way, so a hack is to simply do what NIX_LDFLAGS is telling us we
        # should do always when using a toolchain from Nixpkgs.
        # TODO remove this gross hack.
        args.add("-liconv")

        extra_linker_flags_file = _darwin_create_extra_linker_flags_file(
            hs,
            cc,
            objects_dir,
            executable,
            dynamic,
            cc_solibs,
        )

    if extra_linker_flags_file != None:
        params_file = _merge_parameter_files(hs, objects_dir_manifest, extra_linker_flags_file)
    else:
        params_file = objects_dir_manifest

    hs.toolchain.actions.run_ghc(
        hs,
        cc,
        inputs = depset(transitive = [
            depset(extra_srcs),
            set.to_depset(dep_info.package_databases),
            set.to_depset(dep_info.dynamic_libraries),
            depset(dep_info.static_libraries),
            depset(dep_info.static_libraries_prof),
            depset([objects_dir]),
            cc_link_libs,
        ]),
        outputs = [executable],
        mnemonic = "HaskellLinkBinary",
        arguments = args,
        params_file = params_file,
    )

    return (executable, cc_solibs + hs_solibs)

def _add_external_libraries(args, ext_libs):
    """Add options to `args` that allow us to link to `ext_libs`.

    Args:
      args: Args object.
      ext_libs: C library dependencies.
    """

    # Deduplicate the list of ext_libs based on their
    # library name (file name stripped of lib prefix and endings).
    # This keeps the command lines short, e.g. when a C library
    # like `liblz4.so` appears in multiple dependencies.
    # XXX: this is only done in here
    # Shouldn’t the deduplication be applied to *all* external libraries?
    deduped = list.dedup_on(get_lib_name, ext_libs)

    for lib in deduped:
        args.add_all([
            "-L{0}".format(
                paths.dirname(lib.path),
            ),
            "-l{0}".format(
                # technically this is the second call to get_lib_name,
                #  but the added clarity makes up for it.
                get_lib_name(lib),
            ),
        ])

def _infer_rpaths(is_darwin, target, solibs):
    """Return set of RPATH values to be added to target so it can find all
    solibs

    The resulting paths look like:
    $ORIGIN/../../path/to/solib/dir
    This means: "go upwards to your runfiles directory, then descend into
    the parent folder of the solib".

    Args:
      is_darwin: Whether we're compiling on and for Darwin.
      target: File, executable or library we're linking.
      solibs: A list of Files, shared objects that the target needs.

    Returns:
      Set of strings: rpaths to add to target.
    """
    r = set.empty()

    if is_darwin:
        prefix = "@loader_path"
    else:
        prefix = "$ORIGIN"

    for solib in solibs:
        rpath = create_rpath_entry(
            binary = target,
            dependency = solib,
            keep_filename = False,
            prefix = prefix,
        )
        set.mutable_insert(r, rpath)

    return r

def _so_extension(hs):
    """Returns the extension for shared libraries.

    Args:
      hs: Haskell rule context.

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
    inputs = [objects_dir, objects_dir_manifest] + cc.files

    if hs.toolchain.is_darwin:
        # On Darwin, ar doesn't support params files.
        args.add_all([
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
            command = "{ar} qc $1 $(< $2)".format(ar = cc.tools.ar),
            arguments = [args],

            # Use the default macosx toolchain
            env = {"SDKROOT": "macosx"},
        )
    else:
        args.add_all([
            "qc",
            static_library,
            "@" + objects_dir_manifest.path,
        ])
        hs.actions.run(
            inputs = inputs,
            outputs = [static_library],
            mnemonic = "HaskellLinkStaticLibrary",
            executable = cc.tools.ar,
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
    args.add_all(["-optl" + f for f in cc.linker_flags])
    args.add_all(["-shared", "-dynamic"])

    # Work around macOS linker limits.  This fix has landed in GHC HEAD, but is
    # not yet in a release; plus, we still want to support older versions of
    # GHC.  For details, see: https://phabricator.haskell.org/D4714
    if hs.toolchain.is_darwin:
        args.add("-optl-Wl,-dead_strip_dylibs")

    args.add_all(pkg_info_to_compile_flags(expose_packages(
        dep_info,
        lib_info = None,
        use_direct = True,
        use_my_pkg_id = None,
        custom_package_databases = None,
        version = my_pkg_id.version if my_pkg_id else None,
    )))

    (cc_link_libs, _cc_solibs, _hs_solibs) = _link_dependencies(
        hs = hs,
        dep_info = dep_info,
        dynamic = True,
        binary = dynamic_library,
        args = args,
    )

    args.add_all(["-o", dynamic_library.path])

    # Profiling not supported for dynamic libraries.
    objects_dir_manifest = _create_objects_dir_manifest(
        hs,
        objects_dir,
        dynamic = True,
        with_profiling = False,
    )

    hs.toolchain.actions.run_ghc(
        hs,
        cc,
        inputs = depset([objects_dir], transitive = [
            depset(extra_srcs),
            set.to_depset(dep_info.package_databases),
            set.to_depset(dep_info.dynamic_libraries),
            cc_link_libs,
        ]),
        outputs = [dynamic_library],
        mnemonic = "HaskellLinkDynamicLibrary",
        arguments = args,
        params_file = objects_dir_manifest,
    )

    return dynamic_library
