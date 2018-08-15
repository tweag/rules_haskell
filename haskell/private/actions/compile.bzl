"""Actions for compiling Haskell source code"""

load(":private/java.bzl", "java_interop_info")
load(
    ":private/path_utils.bzl",
    "declare_compiled",
    "get_external_libs_path",
    "target_unique_name",
)
load(":private/pkg_id.bzl", "pkg_id")
load(
    ":private/providers.bzl",
    "C2hsLibraryInfo",
    "DefaultCompileInfo",
)
load(":private/set.bzl", "set")
load("@bazel_skylib//:lib.bzl", "dicts", "paths")

def _process_hsc_file(hs, cc, hsc_file):
    """Process a single hsc file.

    Args:
      hs: Haskell context.
      cc: CcInteropInfo, information about C dependencies.
      hsc_file: hsc file to process.

    Returns:
      (File, string): Haskell source file created by processing hsc_file and
         new import directory containing the produced file.
    """
    args = hs.actions.args()

    # Output a Haskell source file.
    hsc_dir_raw = paths.join("_hsc", hs.name)
    hs_out = declare_compiled(hs, hsc_file, ".hs", directory = hsc_dir_raw)
    args.add([hsc_file.path, "-o", hs_out.path])

    args.add(["-c", hs.tools.cc])
    args.add(["-l", hs.tools.cc])
    args.add("-ighcplatform.h")
    args.add("-ighcversion.h")
    args.add(["--cflag=" + f for f in cc.cpp_flags])
    args.add(["--cflag=" + f for f in cc.compiler_flags])
    args.add(["--cflag=" + f for f in cc.include_args])
    args.add(["--lflag=" + f for f in cc.linker_flags])

    hs.actions.run(
        inputs = depset(transitive = [
            depset(cc.hdrs),
            depset([hs.tools.cc, hsc_file]),
        ]),
        outputs = [hs_out],
        mnemonic = "HaskellHsc2hs",
        executable = hs.tools.hsc2hs,
        arguments = [args],
        env = hs.env,
    )

    idir = paths.join(
        hs.bin_dir.path,
        hs.label.package,
        hsc_dir_raw,
    )

    return hs_out, idir

def _compilation_defaults(hs, cc, java, dep_info, srcs, import_dir_map, extra_srcs, compiler_flags, with_profiling, my_pkg_id = None):
    """Declare default compilation targets and create default compiler arguments.

    Returns:
      DefaultCompileInfo: Populated default compilation settings.
    """

    ghc_args = []

    # GHC expects the CC compiler as the assembler, but segregates the
    # set of flags to pass to it when used as an assembler. So we have
    # to set both -optc and -opta.
    cc_args = [
        "-optc" + f
        for f in cc.compiler_flags
    ] + [
        "-opta" + f
        for f in cc.compiler_flags
    ]
    ghc_args += cc_args

    interface_dir_raw = "_iface_prof" if with_profiling else "_iface"
    object_dir_raw = "_obj_prof" if with_profiling else "_obj"

    # Declare file directories.
    #
    # NOTE: We could have used -outputdir here and a single output
    # directory. But keeping interface and object files separate has
    # one advantage: if interface files are invariant under
    # a particular code change, then we don't need to rebuild
    # downstream.
    if my_pkg_id:
        # If we're compiling a package, put the interfaces inside the
        # package directory.
        interfaces_dir = hs.actions.declare_directory(
            paths.join(
                my_pkg_id.name,
                interface_dir_raw,
            ),
        )
    else:
        interfaces_dir = hs.actions.declare_directory(
            paths.join(interface_dir_raw, hs.name),
        )
    objects_dir = hs.actions.declare_directory(
        paths.join(object_dir_raw, hs.name),
    )

    # Default compiler flags.
    ghc_args += hs.toolchain.compiler_flags
    ghc_args += compiler_flags
    ghc_args.append("-hide-all-packages")

    # Work around macOS linker limits.  This fix has landed in GHC HEAD, but is
    # not yet in a release; plus, we still want to support older versions of
    # GHC.  For details, see: https://phabricator.haskell.org/D4714
    if hs.toolchain.is_darwin:
        ghc_args += ["-optl-Wl,-dead_strip_dylibs"]

    # Expose all prebuilt dependencies
    for prebuilt_dep in set.to_list(dep_info.direct_prebuilt_deps):
        ghc_args += ["-package", prebuilt_dep]

    # Expose all bazel dependencies
    for package in set.to_list(dep_info.package_ids):
        if package != my_pkg_id:
            ghc_args += ["-package-id", package]

    # Only include package DBs for deps, prebuilt deps should be found
    # auto-magically by GHC.
    for cache in set.to_list(dep_info.package_caches):
        ghc_args += ["-package-db", cache.dirname]

    header_files = []
    boot_files = []
    source_files = set.empty()

    # Add import hierarchy root.
    # Note that this is not perfect, since GHC requires hs-boot files
    # to be in the same directory as the corresponding .hs file.  Thus
    # the two must both have the same root; i.e., both plain files,
    # both in bin_dir, or both in genfiles_dir.

    import_dirs = set.from_list([
        hs.src_root,
        paths.join(hs.bin_dir.path, hs.src_root),
        paths.join(hs.genfiles_dir.path, hs.src_root),
    ])

    for s in srcs:
        if s.extension == "h":
            header_files.append(s)
        elif s.extension == "hsc":
            s0, idir = _process_hsc_file(hs, cc, s)
            set.mutable_insert(source_files, s0)
            set.mutable_insert(import_dirs, idir)
        elif s.extension in ["hs-boot", "lhs-boot"]:
            boot_files.append(s)
        else:
            set.mutable_insert(source_files, s)

        if s in import_dir_map:
            idir = import_dir_map[s]
            set.mutable_insert(import_dirs, idir)

    ghc_args += ["-i{0}".format(d) for d in set.to_list(import_dirs)]
    ghc_args += ["-optP" + f for f in cc.cpp_flags]
    ghc_args += cc.include_args

    locale_archive_depset = (
        depset([hs.toolchain.locale_archive]) if hs.toolchain.locale_archive != None else depset()
    )

    # This is absolutely required otherwise GHC doesn't know what package it's
    # creating `Name`s for to put them in Haddock interface files which then
    # results in Haddock not being able to find names for linking in
    # environment after reading its interface file later.
    if my_pkg_id != None:
        unit_id_args = [
            "-this-unit-id",
            my_pkg_id.name,
            "-optP-DCURRENT_PACKAGE_KEY=\"{}\"".format(my_pkg_id.name),
        ]
        ghc_args += unit_id_args

    args = hs.actions.args()

    # Compilation mode.  Allow rule-supplied compiler flags to override it.
    if hs.mode == "opt":
        args.add("-O2")

    args.add(ghc_args)

    args.add(["-static"])

    # NOTE We can't have profiling and dynamic code at the same time, see:
    # https://ghc.haskell.org/trac/ghc/ticket/15394
    if with_profiling:
        args.add("-prof", "-fexternal-interpreter")
    else:
        args.add(["-dynamic-too"])

    # Common flags
    args.add([
        "-v0",
        "-c",
        "--make",
        "-fPIC",
        "-hide-all-packages",
    ])

    # Output directories
    args.add([
        "-odir",
        objects_dir,
        "-hidir",
        interfaces_dir,
    ])

    # Interface files with profiling have to have the extension "p_hi":
    # https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#installedpackageinfo-a-package-specification
    # otherwise we won't be able to register them with ghc-pkg.
    if with_profiling:
        args.add([
            "-hisuf",
            "p_hi",
            "-osuf",
            "p_o",
        ])

    # Pass source files
    for f in set.to_list(source_files):
        args.add(f)

    return DefaultCompileInfo(
        args = args,
        ghc_args = ghc_args,
        inputs = depset(transitive = [
            depset(header_files),
            depset(boot_files),
            set.to_depset(source_files),
            extra_srcs,
            depset(cc.hdrs),
            set.to_depset(dep_info.package_confs),
            set.to_depset(dep_info.package_caches),
            set.to_depset(dep_info.interface_dirs),
            depset(dep_info.static_libraries),
            depset(dep_info.static_libraries_prof),
            set.to_depset(dep_info.dynamic_libraries),
            depset(dep_info.external_libraries.values()),
            java.inputs,
            depset([hs.tools.cc]),
            locale_archive_depset,
        ]),
        objects_dir = objects_dir,
        interfaces_dir = interfaces_dir,
        outputs = [objects_dir, interfaces_dir],
        header_files = set.from_list(cc.hdrs + header_files),
        boot_files = set.from_list(boot_files),
        source_files = source_files,
        extra_source_files = extra_srcs,
        import_dirs = import_dirs,
        env = dicts.add(
            {
                "LD_LIBRARY_PATH": get_external_libs_path(set.from_list(dep_info.external_libraries.values())),
            },
            java.env,
            hs.env,
        ),
    )

def compile_binary(hs, cc, java, dep_info, srcs, ls_modules, import_dir_map, extra_srcs, compiler_flags, with_profiling, main_function):
    """Compile a Haskell target into object files suitable for linking.

    Returns:
      struct with the following fields:
        object_files: list of static object files
        object_dyn_files: list of dynamic object files
        modules: set of module names
        source_files: set of Haskell source files
    """
    c = _compilation_defaults(hs, cc, java, dep_info, srcs, import_dir_map, extra_srcs, compiler_flags, with_profiling)
    c.args.add(["-main-is", main_function])

    hs.toolchain.actions.run_ghc(
        hs,
        inputs = c.inputs + hs.extra_binaries,
        outputs = c.outputs,
        mnemonic = "HaskellBuildBinary",
        progress_message = "HaskellBuildBinary {}".format(hs.label),
        env = c.env,
        arguments = [c.args],
    )

    if with_profiling:
        exposed_modules_file = None
    else:
        exposed_modules_file = hs.actions.declare_file(
            target_unique_name(hs, "exposed-modules"),
        )
        hs.actions.run(
            inputs = [c.interfaces_dir, hs.toolchain.global_pkg_db],
            outputs = [exposed_modules_file],
            executable = ls_modules,
            arguments = [
                c.interfaces_dir.path,
                hs.toolchain.global_pkg_db.path,
                "/dev/null",  # no hidden modules
                "/dev/null",  # no reexported modules
                exposed_modules_file.path,
            ],
            use_default_shell_env = True,
        )

    return struct(
        objects_dir = c.objects_dir,
        source_files = c.source_files,
        import_dirs = c.import_dirs,
        ghc_args = c.ghc_args,
        header_files = c.header_files,
        exposed_modules_file = exposed_modules_file,
    )

def compile_library(hs, cc, java, dep_info, srcs, ls_modules, other_modules, exposed_modules_reexports, import_dir_map, extra_srcs, compiler_flags, with_profiling, my_pkg_id):
    """Build arguments for Haskell package build.

    Returns:
      struct with the following fields:
        interfaces_dir: directory containing interface files
        interface_files: list of interface files
        object_files: list of static object files
        object_dyn_files: list of dynamic object files
        ghc_args: list of string arguments suitable for Haddock
        modules: set of module names
        source_files: set of Haskell module files
        import_dirs: import directories that should make all modules visible (for GHCi)
    """
    c = _compilation_defaults(hs, cc, java, dep_info, srcs, import_dir_map, extra_srcs, compiler_flags, with_profiling, my_pkg_id = my_pkg_id)

    hs.toolchain.actions.run_ghc(
        hs,
        inputs = c.inputs + hs.extra_binaries,
        outputs = c.outputs,
        mnemonic = "HaskellBuildLibrary",
        progress_message = "HaskellBuildLibrary {}".format(hs.label),
        env = c.env,
        arguments = [c.args],
    )

    if with_profiling:
        exposed_modules_file = None
    else:
        hidden_modules_file = hs.actions.declare_file(
            target_unique_name(hs, "hidden-modules"),
        )
        hs.actions.write(
            output = hidden_modules_file,
            content = ", ".join(other_modules),
        )
        reexported_modules_file = hs.actions.declare_file(
            target_unique_name(hs, "reexported-modules"),
        )
        hs.actions.write(
            output = reexported_modules_file,
            content = ", ".join(exposed_modules_reexports),
        )
        exposed_modules_file = hs.actions.declare_file(
            target_unique_name(hs, "exposed-modules"),
        )
        hs.actions.run(
            inputs = [
                c.interfaces_dir,
                hs.toolchain.global_pkg_db,
                hidden_modules_file,
                reexported_modules_file,
            ],
            outputs = [exposed_modules_file],
            executable = ls_modules,
            arguments = [
                c.interfaces_dir.path,
                hs.toolchain.global_pkg_db.path,
                hidden_modules_file.path,
                reexported_modules_file.path,
                exposed_modules_file.path,
            ],
            use_default_shell_env = True,
        )

    return struct(
        interfaces_dir = c.interfaces_dir,
        objects_dir = c.objects_dir,
        ghc_args = c.ghc_args,
        header_files = c.header_files,
        boot_files = c.boot_files,
        source_files = c.source_files,
        extra_source_files = c.extra_source_files,
        import_dirs = c.import_dirs,
        exposed_modules_file = exposed_modules_file,
    )
