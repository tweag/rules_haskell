"""Actions for linking object code produced by compilation"""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load(":private/packages.bzl", "expose_packages", "pkg_info_to_compile_flags")
load(":private/pkg_id.bzl", "pkg_id")
load(":private/cc_libraries.bzl", "create_link_config")

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

def _create_objects_dir_manifest(hs, posix, objects_dir, extra_objects, dynamic, with_profiling):
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

        # Note: The output of `find` is not stable. The order of the
        # lines in the output depend on the filesystem. By using
        # `sort`, we force the output to be stable. This is mandatory
        # for efficient caching. See
        # https://github.com/tweag/rules_haskell/issues/1126.
        command = """
        "{cat}" <("{find}" {dir} -name '*.{ext}') <(echo -n "{extra}") | "{sort}" > {out}
        """.format(
            find = posix.commands["find"],
            sort = posix.commands["sort"],
            cat = posix.commands["cat"],
            dir = objects_dir.path,
            ext = ext,
            extra = "\n".join([e.path for e in extra_objects]),
            out = objects_dir_manifest.path,
        ),
    )

    return objects_dir_manifest

def link_binary(
        hs,
        cc,
        posix,
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

    objects_dir_manifest = _create_objects_dir_manifest(
        hs,
        posix,
        objects_dir,
        [],
        dynamic = dynamic,
        with_profiling = with_profiling,
    )

    if hs.toolchain.is_darwin:
        args.add("-optl-Wl,-headerpad_max_install_names")

        # Nixpkgs commit 3513034208a introduces -liconv in NIX_LDFLAGS on
        # Darwin. We don't currently handle NIX_LDFLAGS in any special
        # way, so a hack is to simply do what NIX_LDFLAGS is telling us we
        # should do always when using a toolchain from Nixpkgs.
        # TODO remove this gross hack.
        args.add("-liconv")

    hs.toolchain.actions.run_ghc(
        hs,
        cc,
        inputs = depset(transitive = [
            depset(extra_srcs),
            dep_info.package_databases,
            dep_info.hs_libraries,
            depset([cache_file, objects_dir]),
            pkg_info_inputs,
            depset(static_libs + dynamic_libs),
        ]),
        outputs = [executable],
        mnemonic = "HaskellLinkBinary",
        arguments = args,
        params_file = objects_dir_manifest,
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
# must take care to only pass -no-pie when compiling libraries.
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

def link_library_static(hs, cc, posix, dep_info, objects_dir, extra_objects, my_pkg_id, with_profiling):
    """Link a static library for the package using given object files.

    Returns:
      File: Produced static library.
    """
    static_library = hs.actions.declare_file(
        "lib{0}.a".format(pkg_id.library_name(hs, my_pkg_id, prof_suffix = with_profiling)),
    )
    objects_dir_manifest = _create_objects_dir_manifest(
        hs,
        posix,
        objects_dir,
        extra_objects,
        dynamic = False,
        with_profiling = with_profiling,
    )
    args = hs.actions.args()
    inputs = [objects_dir, objects_dir_manifest] + extra_objects + cc.files

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

def link_library_dynamic(hs, cc, posix, dep_info, extra_srcs, objects_dir, extra_objects, my_pkg_id, compiler_flags):
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
    args.add_all(hs.toolchain.ghcopts)
    args.add_all(compiler_flags)

    (pkg_info_inputs, pkg_info_args) = pkg_info_to_compile_flags(
        hs,
        pkg_info = expose_packages(
            package_ids = hs.package_ids,
            package_databases = dep_info.package_databases,
            version = my_pkg_id.version if my_pkg_id else None,
        ),
        prefix = "link-",
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
    )

    args.add_all(["-o", dynamic_library.path])

    # Profiling not supported for dynamic libraries.
    objects_dir_manifest = _create_objects_dir_manifest(
        hs,
        posix,
        objects_dir,
        extra_objects,
        dynamic = True,
        with_profiling = False,
    )

    hs.toolchain.actions.run_ghc(
        hs,
        cc,
        inputs = depset([cache_file, objects_dir] + extra_objects, transitive = [
            extra_srcs,
            dep_info.package_databases,
            dep_info.hs_libraries,
            pkg_info_inputs,
            depset(static_libs + dynamic_libs),
        ]),
        outputs = [dynamic_library],
        mnemonic = "HaskellLinkDynamicLibrary",
        arguments = args,
        params_file = objects_dir_manifest,
        env = dicts.add(hs.env, cc.env),
    )

    return dynamic_library
