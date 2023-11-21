"""Actions for linking object code produced by compilation"""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
load(":private/packages.bzl", "expose_packages", "pkg_info_to_compile_flags")
load(":private/pkg_id.bzl", "pkg_id")
load(":private/cc_libraries.bzl", "create_link_config", "get_cc_libraries", "get_library_files")

def merge_parameter_files(hs, file1, file2):
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

def darwin_flags_for_linking_indirect_cc_deps(hs, cc, posix, basename, dynamic):
    """Write flags to force linking cc dependencies on MacOS to a parameter file.

    GHC uses -dead_strip_dylibs in MacOS which discards all of the
    indirect dependencies. Since cc libraries are underlinked, we use
    -u symbol flags to force the linker to keep the cc dependencies
    even when -dead_strip_dylibs is used.

    Args:
      hs: Haskell context.
      cc: CcInteropInfo, information about C dependencies.
      posix: Posix toolchain
      basename: The basename to use for the output file.
      dynamic: Bool: Whether to link dynamically or statically.

    Returns:
      File: Parameter file with additional linker flags to be passed to GHC,
      or None if we aren't building for darwin.
    """

    if not hs.toolchain.is_darwin:
        return None

    # On Darwin GHC will pass the dead_strip_dylibs flag to the linker. This
    # flag will remove any shared library loads from the binary's header that
    # are not directly resolving undefined symbols in the binary. I.e. any
    # indirect shared library dependencies will be removed. This conflicts with
    # Bazel's builtin cc rules, which assume that the final binary will load
    # all transitive shared library dependencies. In particlar shared libraries
    # produced by Bazel's cc rules never load shared libraries themselves. This
    # causes missing symbols at runtime on MacOS, see #170.
    #
    # The following work-around applies the `-u` flag to the linker for an external
    # symbol of each cc dependency. This forces the linker to resolve these
    # undefined symbols in all the transitive shared cc library dependencies
    # and keep the corresponding load commands in the binary's header.
    suffix = ".dynamic.linker_flags" if dynamic else ".static.linker_flags"
    linker_flags_file = hs.actions.declare_file(basename + suffix)

    (_cc_static_libs, cc_dynamic_libs) = get_library_files(
        hs,
        cc.cc_libraries_info,
        get_cc_libraries(cc.cc_libraries_info, cc.transitive_libraries),
        dynamic,
    )

    # Some flags used in the invocation to nm are MacOS-specific and
    # tested to exist at least since 10.15.
    #
    # -g     Display only global (external) symbols.
    # -j     Just display the symbol names (no value or type).
    # -p     Don't sort; display in symbol-table order.
    # -U     Don't display undefined symbols.
    hs.actions.run_shell(
        inputs = cc_dynamic_libs,
        outputs = [linker_flags_file],
        command = """
        touch {out}
        for lib in {solibs}; do
            {nm} -Ujpg "$lib" | {head} -1 | {sed} 's/^/-u /' >> {out}
        done
        """.format(
            nm = cc.tools.nm,
            head = posix.commands["head"],
            sed = posix.commands["sed"],
            solibs = " ".join(["\"" + l.path + "\"" for l in cc_dynamic_libs]),
            out = linker_flags_file.path,
        ),
    )
    return linker_flags_file

def link_binary(
        hs,
        cc,
        posix,
        dep_info,
        extra_srcs,
        compiler_flags,
        object_files,
        extra_objects,
        extra_ldflags_file,
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
    args.add_all(cc.linker_flags, format_each = "-optl%s")
    if with_profiling:
        args.add("-prof")
    args.add_all(hs.toolchain.ghcopts)
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
    elif not hs.toolchain.is_darwin and not hs.toolchain.is_windows:
        # See Note [No PIE when linking]
        if hs.toolchain.numeric_version < [8, 10]:
            args.add("-optl-no-pie")

    # When compiling with `-threaded`, GHC needs to link against
    # the pthread library when linking against static archives (.a).
    # We assume it’s not a problem to pass it for other cases,
    # so we just default to passing it.
    args.add("-optl-pthread")

    if hs.features.fully_static_link:
        # Create a fully statically linked binary.
        args.add("-optl-static")

    args.add_all(["-o", executable.path])

    (pkg_info_inputs, pkg_info_args) = pkg_info_to_compile_flags(
        hs,
        pkg_info = expose_packages(
            package_ids = hs.package_ids,
            package_databases = dep_info.package_databases,
            version = version,
        ),
        prefix = "link-",
    )
    args.add_all(pkg_info_args)

    (cache_file, static_libs, dynamic_libs) = create_link_config(
        hs = hs,
        posix = posix,
        cc_libraries_info = cc.cc_libraries_info,
        libraries_to_link = cc.transitive_libraries,
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

    args.add_all(object_files)
    args.add_all(extra_objects)

    if hs.toolchain.is_darwin:
        args.add("-optl-Wl,-headerpad_max_install_names")

        # Nixpkgs commit 3513034208a introduces -liconv in NIX_LDFLAGS on
        # Darwin. We don't currently handle NIX_LDFLAGS in any special
        # way, so a hack is to simply do what NIX_LDFLAGS is telling us we
        # should do always when using a toolchain from Nixpkgs.
        # TODO remove this gross hack.
        args.add("-liconv")

    input_files = [cache_file] + object_files
    if extra_ldflags_file:
        args.add("-optl@{}".format(extra_ldflags_file.path))
        input_files.append(extra_ldflags_file)

    hs.toolchain.actions.run_ghc(
        hs,
        cc,
        inputs = depset(transitive = [
            depset(extra_srcs),
            dep_info.package_databases,
            dep_info.hs_libraries,
            depset(input_files, transitive = [extra_objects]),
            pkg_info_inputs,
            depset(static_libs + dynamic_libs),
        ]),
        outputs = [executable],
        mnemonic = "HaskellLinkBinary",
        arguments = args,
        env = dicts.add(hs.env, cc.env),
    )

    return (executable, dynamic_libs)

# Note [No PIE while linking]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# GHC links object files with the `ld` flag `-r` and compiles the `main`
# function with non-position independent code when generating an executable.
# On some more recent Linux distributions (e.g. Ubuntu 18.04) `gcc` defaults
# to linking with `-pie` which is incompatible with both behaviors described
# above.
#
# Combining `-pie` and `ld -r` causes errors of the form:
#
#     /usr/bin/ld: -r and -pie may not be used together
#
# Compiling executables with `-pie` leads to errors of the form:
#
#   /usr/bin/ld.gold: error: /tmp/ghc3_0/ghc_2.o: requires dynamic R_X86_64_32 reloc against 'ZCMain_main_closure' which may overflow at runtime; recompile with -fPIC
#   /usr/bin/ld.gold: error: ../../../../../external/rules_haskell_ghc_linux_amd64/lib/base-4.13.0.0/libHSbase-4.13.0.0.a(Base.o): requires unsupported dynamic reloc 11; recompile with -fPIC
#
# GHC determines whether the C compiler supports the `-no-pie` flag during
# `./configure` and stores this information in its `settings` file. Depending
# on this setting GHC would then automatically set `-no-pie`. However,
# rules_haskell uses GHC's `-pgmc` flag to point GHC to the CC toolchain's C
# compiler. This disables the flags configured in the `setting` file. Instead,
# we have to pass `-no-pie` explicitly when linking binaries.
#
# In GHC < 8.10, we must always pass -no-pie. In newer compilers, we
# must never pass -no-pie, at any rate not when linking binaries.
#
# Ideally, we would determine whether the CC toolchain's C compiler supports
# `-no-pie` before setting it. Unfortunately, this is complicated by the fact
# that Bazel does not support dynamic dependencies in build actions and that
# repository rules don't have access to toolchains, yet.
#
# If necessary this flag could be made user configurable at the level of the
# GHC toolchain or a wrapper around the GHC compiler could determine if
# the C compiler supports `-no-pie`.
#
# Further information:
#
# - GHC Note [No PIE while linking] https://gitlab.haskell.org/ghc/ghc/-/blob/d0bab2e3419e49cdbb1201d4650572b57f33420c/compiler/main/DynFlags.hs#L5551-5557
# - https://gitlab.haskell.org/ghc/ghc/-/issues/12759
# - https://gitlab.haskell.org/ghc/ghc/-/issues/15319

def _so_extension(hs):
    """Returns the extension for shared libraries.

    Args:
      hs: Haskell rule context.

    Returns:
      string of extension.
    """
    return "dylib" if hs.toolchain.is_darwin else "so"

def link_library_static(hs, cc, _posix, _dep_info, object_files, my_pkg_id, with_profiling, libdir = ""):
    """Link a static library for the package using given object files.

    Returns:
      File: Produced static library.
    """
    static_library = hs.actions.declare_file(
        paths.join(
            libdir,
            "lib{0}.a".format(pkg_id.library_name(hs, my_pkg_id, prof_suffix = with_profiling)),
        ),
    )
    inputs = depset(cc.files, transitive = [object_files])
    args = hs.actions.args()
    args.add_all(["qc", static_library])
    args.add_all(object_files)

    if hs.toolchain.is_darwin:
        # TODO Get ar location from the CC toolchain. This is
        # complicated by the fact that the CC toolchain does not
        # always use ar, and libtool has an entirely different CLI.
        # See https://github.com/bazelbuild/bazel/issues/5127
        hs.actions.run_shell(
            inputs = inputs,
            outputs = [static_library],
            mnemonic = "HaskellLinkStaticLibrary",
            command = "{ar} $@".format(ar = cc.tools.ar),
            arguments = [args],

            # Use the default macosx toolchain
            env = {"SDKROOT": "macosx"},
        )
    else:
        hs.actions.run(
            inputs = inputs,
            outputs = [static_library],
            mnemonic = "HaskellLinkStaticLibrary",
            executable = cc.tools.ar,
            arguments = [args],
        )

    return static_library

def dynamic_library_filename(hs, my_pkg_id):
    return "lib{0}-ghc{1}.{2}".format(
        pkg_id.library_name(hs, my_pkg_id),
        hs.toolchain.version,
        _so_extension(hs),
    )

def link_library_dynamic(hs, cc, posix, dep_info, extra_srcs, object_files, my_pkg_id, compiler_flags, extra_ldflags_file, empty_lib_prefix = ""):
    """Link a dynamic library for the package using given object files.

    Returns:
      File: Produced dynamic library.
    """

    dynamic_library = hs.actions.declare_file(
        paths.join(empty_lib_prefix, dynamic_library_filename(hs, my_pkg_id)),
    )

    args = hs.actions.args()
    args.add_all(cc.linker_flags, format_each = "-optl%s")
    args.add_all(["-shared", "-dynamic"])
    args.add_all(hs.toolchain.ghcopts)
    args.add_all(compiler_flags)
    extra_prefix = empty_lib_prefix

    (pkg_info_inputs, pkg_info_args) = pkg_info_to_compile_flags(
        hs,
        pkg_info = expose_packages(
            package_ids = [] if empty_lib_prefix else hs.package_ids,
            package_databases = dep_info.package_databases,
            version = my_pkg_id.version if my_pkg_id else None,
        ),
        prefix = "link-" + extra_prefix + "-",
    )
    args.add_all(pkg_info_args)

    (cache_file, static_libs, dynamic_libs) = create_link_config(
        hs = hs,
        posix = posix,
        cc_libraries_info = cc.cc_libraries_info,
        libraries_to_link = cc.transitive_libraries,
        dynamic = True,
        pic = True,
        binary = dynamic_library,
        args = args,
        dirprefix = empty_lib_prefix,
    )

    args.add_all(["-o", dynamic_library.path])

    args.add_all(object_files)

    input_files = [cache_file]
    if extra_ldflags_file:
        args.add("-optl@{}".format(extra_ldflags_file.path))
        input_files.append(extra_ldflags_file)

    hs.toolchain.actions.run_ghc(
        hs,
        cc,
        inputs = depset(input_files, transitive = [
            object_files,
            extra_srcs,
            dep_info.package_databases,
            dep_info.hs_libraries,
            pkg_info_inputs,
            depset(static_libs + dynamic_libs),
        ]),
        outputs = [dynamic_library],
        mnemonic = "HaskellLinkDynamicLibrary",
        arguments = args,
        env = dicts.add(hs.env, cc.env),
        extra_name = "__" + extra_prefix,
    )

    return dynamic_library
