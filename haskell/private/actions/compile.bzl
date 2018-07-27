"""Actions for compiling Haskell source code"""

load(":private/java.bzl", "java_interop_info")
load(
    ":private/path_utils.bzl",
    "declare_compiled",
    "get_external_libs_path",
    "module_name",
    "module_unique_name",
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
    hsc_dir_raw = target_unique_name(hs, "hsc")
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

def _output_file_ext(base, dynamic, profiling_enabled):
    """Return extension that output of compilation should have depending on the
    following inputs:

    Args:
      base: usually "o" for object files and "hi" for interface files. Preceding
        dot "." will be preserved in the output.
      dynamic: bool, whether we're compiling dynamic object files.
      profiling_enabled: bool, whether profiling is enabled.

    Returns:
      String, extension of Haskell object file.
    """

    with_dot = False
    ext = ""

    if base[0] == ".":
        with_dot = True
        ext = base[1:]
    else:
        ext = base

    if dynamic:
        ext = "dyn_" + ext
    if profiling_enabled:
        ext = "p_" + ext
    return ("." if with_dot else "") + ext

def _compilation_defaults(hs, cc, java, dep_info, srcs, import_dir_map, extra_srcs, compiler_flags, with_profiling, main_file = None, my_pkg_id = None):
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

    # Declare file directories
    objects_dir_raw = target_unique_name(hs, "objects")
    objects_dir = paths.join(
        hs.bin_dir.path,
        hs.label.workspace_root,
        hs.label.package,
        objects_dir_raw,
    )
    interfaces_dir_raw = target_unique_name(hs, "interfaces")
    interfaces_dir = paths.join(
        hs.bin_dir.path,
        hs.label.workspace_root,
        hs.label.package,
        interfaces_dir_raw,
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

    # We want object and dynamic objects from all inputs.
    object_files = []
    object_dyn_files = []

    # We need to keep interface files we produce so we can import
    # modules cross-package.
    interface_files = []
    header_files = []
    boot_files = []
    source_files = set.empty()
    modules = set.empty()

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

    # Output object files are named after modules, not after input file names.
    # The difference is only visible in the case of Main module because it may
    # be placed in a file with a name different from "Main.hs". In that case
    # still Main.o will be produced.

    for s in srcs:
        if s.extension == "h":
            header_files.append(s)
        if s.extension in ["hs-boot", "lhs-boot"]:
            boot_files.append(s)
        elif s.extension in ["hs", "lhs", "hsc", "chs"]:
            if not main_file or s != main_file:
                if s.extension == "hsc":
                    s0, idir = _process_hsc_file(hs, cc, s)
                    set.mutable_insert(source_files, s0)
                    set.mutable_insert(import_dirs, idir)
                else:
                    set.mutable_insert(source_files, s)

                rel_path = None

                if s in import_dir_map:
                    idir = import_dir_map[s]
                    set.mutable_insert(import_dirs, idir)
                    rel_path = paths.relativize(s.path, idir)
                    mname = module_name(hs, s, rel_path = rel_path)
                else:
                    mname = module_name(hs, s)

                set.mutable_insert(modules, mname)

                for dynamic in [False] if with_profiling else [True, False]:
                    file_sets = [
                        (interface_files, interfaces_dir_raw, ".hi"),
                        (object_dyn_files if dynamic else object_files, objects_dir_raw, ".o"),
                    ]
                    for (file_set, dir_raw, file_ext) in file_sets:
                        file_set.append(
                            declare_compiled(
                                hs,
                                s,
                                _output_file_ext(file_ext, dynamic, with_profiling),
                                directory = dir_raw,
                                rel_path = rel_path,
                            ),
                        )
            else:
                if s.extension == "hsc":
                    s0 = _process_hsc_file(hs, cc, s)
                    set.mutable_insert(source_files, s0)
                else:
                    set.mutable_insert(source_files, s)

                set.mutable_insert(modules, "Main")

                for dynamic in [False] if with_profiling else [True, False]:
                    file_sets = [
                        (interface_files, interfaces_dir_raw, ".hi"),
                        (object_dyn_files if dynamic else object_files, objects_dir_raw, ".o"),
                    ]
                    for (file_set, dir_raw, file_ext) in file_sets:
                        file_set.append(
                            hs.actions.declare_file(
                                paths.join(
                                    dir_raw,
                                    paths.replace_extension(
                                        "Main",
                                        _output_file_ext(file_ext, dynamic, with_profiling),
                                    ),
                                ),
                            ),
                        )

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
            pkg_id.to_string(my_pkg_id),
            "-optP-DCURRENT_PACKAGE_KEY=\"{}\"".format(pkg_id.to_string(my_pkg_id)),
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
        args.add("-prof")
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

    # Output file extensions
    args.add([
        "-osuf",
        _output_file_ext("o", False, with_profiling),
        "-dynosuf",
        _output_file_ext("o", True, with_profiling),
        "-hisuf",
        _output_file_ext("hi", False, with_profiling),
        "-dynhisuf",
        _output_file_ext("hi", True, with_profiling),
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
            set.to_depset(dep_info.interface_files),
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
        outputs = object_files + object_dyn_files + interface_files,
        object_files = object_files,
        object_dyn_files = object_dyn_files,
        interface_files = interface_files,
        modules = modules,
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

def compile_binary(hs, cc, java, dep_info, srcs, import_dir_map, extra_srcs, compiler_flags, with_profiling, main_file, main_function):
    """Compile a Haskell target into object files suitable for linking.

    Returns:
      struct with the following fields:
        object_files: list of static object files
        object_dyn_files: list of dynamic object files
        modules: set of module names
        source_files: set of Haskell source files
    """
    c = _compilation_defaults(hs, cc, java, dep_info, srcs, import_dir_map, extra_srcs, compiler_flags, with_profiling, main_file = main_file)
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

    return struct(
        object_files = c.object_files,
        object_dyn_files = c.object_dyn_files,
        modules = c.modules,
        source_files = c.source_files,
        import_dirs = c.import_dirs,
        ghc_args = c.ghc_args,
        header_files = c.header_files,
    )

def compile_library(hs, cc, java, dep_info, srcs, import_dir_map, extra_srcs, compiler_flags, with_profiling, my_pkg_id):
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

    return struct(
        interfaces_dir = c.interfaces_dir,
        interface_files = c.interface_files,
        object_files = c.object_files,
        object_dyn_files = c.object_dyn_files,
        ghc_args = c.ghc_args,
        modules = c.modules,
        header_files = c.header_files,
        boot_files = c.boot_files,
        source_files = c.source_files,
        extra_source_files = c.extra_source_files,
        import_dirs = c.import_dirs,
    )
