"""Actions for compiling Haskell source code"""

load(
    ":private/packages.bzl",
    "expose_packages",
    "pkg_info_to_compile_flags",
)
load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    ":private/path_utils.bzl",
    "declare_compiled",
    "target_unique_name",
)
load(":private/pkg_id.bzl", "pkg_id")
load(":private/version_macros.bzl", "version_macro_includes")
load(
    ":providers.bzl",
    "HaskellLibraryInfo",
    "all_dependencies_package_ids",
)
load(
    ":private/cc_libraries.bzl",
    "get_ghci_library_files",
    "link_libraries",
)
load(":private/set.bzl", "set")
load("//haskell/experimental:providers.bzl", "HaskellModuleInfo")

def _process_hsc_file(hs, cc, hsc_flags, hsc_inputs, hsc_file):
    """Process a single hsc file.

    Args:
      hs: Haskell context.
      cc: CcInteropInfo, information about C dependencies.
      hsc_flags: extra flags to pass to hsc2hs
      hsc_inputs: extra file inputs for the hsc2hs command
      hsc_file: hsc file to process.

    Returns:
      (File, string): Haskell source file created by processing hsc_file and
         new import directory containing the produced file.
    """
    args = hs.actions.args()

    # Output a Haskell source file.
    hsc_dir_raw = paths.join("_hsc", hs.name)
    hs_out = declare_compiled(hs, hsc_file, ".hs", directory = hsc_dir_raw)
    args.add_all([hsc_file.path, "-o", hs_out.path])

    args.add_all(["-c", cc.tools.cc])
    args.add_all(["-l", cc.tools.cc])
    args.add("-ighcplatform.h")
    args.add("-ighcversion.h")
    args.add_all(cc.cpp_flags, format_each = "--cflag=%s")
    args.add_all(cc.compiler_flags, format_each = "--cflag=%s")
    args.add_all(cc.include_args, format_each = "--cflag=%s")
    args.add_all(cc.linker_flags, format_each = "--lflag=%s")

    # If are building fully-statically-linked binaries, we need to ensure that
    # we pass arguments to `hsc2hs` such that objects it builds are statically
    # linked, otherwise we'll get dynamic linking errors when trying to execute
    # those objects to generate code as part of the build.  Since the static
    # configuration should ensure that all the objects involved are themselves
    # statically built, this is just a case of passing `-static` to the linker
    # used by `hsc2hs` (which will be our own wrapper script which eventually
    # calls `gcc`, etc.).
    #
    # Note that we also do this in our Cabal wrapper, where `hsc2hs` might be
    # called by Cabal as part of the build process.
    if hs.toolchain.fully_static_link:
        args.add("--lflag=-static")

    args.add_all(hsc_flags)

    # Add an empty PATH variable if not already specified in hs.env.
    # Needed to avoid a "Couldn't read PATH" error on Windows.
    #
    # On Unix platforms, though, we musn't set PATH as it is automatically set up
    # by the run action, unless already set in the env parameter. This triggers
    # build errors when using GHC bindists on Linux.
    if hs.env.get("PATH") == None and hs.toolchain.is_windows:
        hs.env["PATH"] = ""

    hs.actions.run(
        inputs = depset(transitive = [
            depset(cc.hdrs),
            depset([hsc_file]),
            depset(cc.files),
            depset(hsc_inputs),
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

def _compilation_defaults(
    hs,
    cc,
    java,
    posix,
    dep_info,
    th_dep_info,
    plugin_dep_info,
    srcs,
    module_map,
    import_dir_map,
    extra_srcs,
    user_compile_flags,
    output_mode,
    with_profiling,
    interfaces_dir,
    objects_dir,
    my_pkg_id,
    version,
    plugins,
    non_default_plugins,
    preprocessors):
    """Compute variables common to all compilation targets (binary and library).

    Returns:
      struct with the following fields:
        args: default argument list
        compile_flags: arguments that were used to compile the package
        inputs: default inputs
        input_manifests: input manifests
        outputs: default outputs
        object_files: object files
        dyn_object_files: dynamic object files (*.dyn_o)
        interface_files: interface files
        source_files: set of files that contain Haskell modules
        boot_files: set of Haskell boot files
        extra_source_files: depset of non-Haskell source files
        import_dirs: c2hs Import hierarchy roots
        env: default environment variables
    """

    compile_flags = []

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
    compile_flags += cc_args

    interfaces_dir_flag = paths.join(hs.bin_dir.path, hs.package_root, interfaces_dir)
    objects_dir_flag = paths.join(hs.bin_dir.path, hs.package_root, objects_dir)

    # Determine file extensions.
    dyn_object_exts = []
    if with_profiling:
        interface_exts = [".p_hi"]
        object_exts = [".p_o"]
    elif output_mode == "dynamic-too":
        interface_exts = [".hi", ".dyn_hi"]
        object_exts = [".o"]
        dyn_object_exts = [".dyn_o"]
    elif output_mode == "dynamic":
        interface_exts = [".hi"]
        object_exts = []
        dyn_object_exts = [".dyn_o"]
    else:
        interface_exts = [".hi"]
        object_exts = [".o"]

    # Declare output files.
    interface_files = [
        hs.actions.declare_file(paths.join(interfaces_dir, filename))
        for (module_name, module_info) in module_map.items()
        for interface_ext in interface_exts
        for filename_prefix in [module_name.replace(".", "/") + interface_ext]
        for filename in [filename_prefix, filename_prefix + "-boot" if module_info.boot else None]
        if filename
    ]
    object_files = [
        hs.actions.declare_file(paths.join(objects_dir, filename))
        for module_name in module_map.keys()
        for object_ext in object_exts
        for filename in [module_name.replace(".", "/") + object_ext]
    ]
    dyn_object_files = [
        hs.actions.declare_file(paths.join(objects_dir, filename))
        for module_name in module_map.keys()
        for object_ext in dyn_object_exts
        for filename in [module_name.replace(".", "/") + object_ext]
    ]

    # Default compiler flags.
    compile_flags += hs.toolchain.ghcopts
    compile_flags += user_compile_flags

    package_ids = []
    all_plugins = plugins + non_default_plugins
    for plugin in all_plugins:
        package_ids.extend(all_dependencies_package_ids(plugin.deps))

    (pkg_info_inputs, pkg_info_args) = pkg_info_to_compile_flags(
        hs,
        pkg_info = expose_packages(
            package_ids = set.to_list(
                set.from_list(
                    hs.package_ids + hs.th_package_ids
                )
            ),
            package_databases = depset(transitive=[
                dep_info.package_databases,
                th_dep_info.package_databases,
            ]),
            version = version,
        ),
        plugin_pkg_info = expose_packages(
            package_ids = package_ids,
            package_databases = plugin_dep_info.package_databases,
            version = version,
        ),
        prefix = "compile-",
    )
    compile_flags.extend(pkg_info_args)

    # Forward all "-D" and "-optP-D" flags to hsc2hs
    hsc_flags = []
    hsc_flags += ["--cflag=" + x for x in user_compile_flags if x.startswith("-D")]
    hsc_flags += ["--cflag=" + x[len("-optP"):] for x in user_compile_flags if x.startswith("-optP-D")]

    hsc_inputs = []
    if version:
        for info in [dep_info, th_dep_info]:
            (version_macro_headers, version_macro_flags) = version_macro_includes(info)
            hsc_flags += ["--cflag=" + x for x in version_macro_flags]
            hsc_inputs += set.to_list(version_macro_headers)

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

    header_files = []
    boot_files = []
    source_files = []
    for s in srcs:
        if s.extension == "h":
            header_files.append(s)
        elif s.extension == "hsc":
            s0, idir = _process_hsc_file(hs, cc, hsc_flags, hsc_inputs, s)
            source_files.append(s0)
            set.mutable_insert(import_dirs, idir)
        elif s.extension in ["hs-boot", "lhs-boot"]:
            boot_files.append(s)
        else:
            source_files.append(s)

        if s in import_dir_map:
            idir = import_dir_map[s]
            set.mutable_insert(import_dirs, idir)

    # Write the -optP flags to a parameter file because they can be very long on Windows
    # e.g. 27Kb for grpc-haskell
    # Equivalent to: compile_flags += ["-optP" + f for f in cc.cpp_flags]
    optp_args_file = hs.actions.declare_file("optp_args_%s" % hs.name)
    optp_args = hs.actions.args()
    optp_args.add_all(cc.cpp_flags)
    optp_args.set_param_file_format("multiline")
    hs.actions.write(optp_args_file, optp_args)
    compile_flags += ["-optP@" + optp_args_file.path]

    compile_flags += cc.include_args

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
        compile_flags += unit_id_args

    args = hs.actions.args()

    # Compilation mode.  Allow rule-supplied compiler flags to override it.
    if hs.mode == "opt":
        args.add("-O2")

    args.add("-static")
    if with_profiling:
        args.add("-prof", "-fexternal-interpreter")

    # Common flags
    args.add_all([
        "-v0",
        "-no-link",
        "-fPIC",
        "-hide-all-packages",
        # Should never trigger in sandboxed builds, but can be useful
        # to debug issues in non-sandboxed builds.
        "-Wmissing-home-modules",
    ])
    if hs.toolchain.static_runtime and not hs.toolchain.is_windows:
        # A static GHC RTS requires -fPIC. However, on Unix we also require
        # -fexternal-dynamic-refs, otherwise GHC still generates R_X86_64_PC32
        # relocations which prevents loading these static libraries as PIC.
        args.add("-fexternal-dynamic-refs")

    # Output directories
    args.add_all([
        "-odir",
        objects_dir_flag,
        "-hidir",
        interfaces_dir_flag,
    ])

    # Interface files with profiling have to have the extension "p_hi":
    # https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#installedpackageinfo-a-package-specification
    # otherwise we won't be able to register them with ghc-pkg.
    if with_profiling:
        args.add_all([
            "-hisuf",
            "p_hi",
            "-osuf",
            "p_o",
        ])

    args.add_all(compile_flags)

    # Plugins
    for plugin in plugins:
        args.add("-fplugin={}".format(plugin.module))
    for plugin in all_plugins:
        for opt in plugin.args:
            args.add_all(["-fplugin-opt", "{}:{}".format(plugin.module, opt)])

    plugin_tool_inputs = depset(transitive = [plugin.tool_inputs for plugin in all_plugins])
    plugin_tool_input_manifests = [
        manifest
        for plugin in all_plugins
        for manifest in plugin.tool_input_manifests
    ]

    # Pass source files
    args.add_all(source_files)

    source_files = depset(source_files)
    header_files = depset(header_files)
    boot_files = depset(boot_files)
    extra_source_files = depset(
        transitive = [
            extra_srcs,
            header_files,
            pkg_info_inputs,
            depset([optp_args_file]),
        ],
    )

    # Transitive library dependencies for runtime.
    link_libraries(
        get_ghci_library_files(hs, cc.cc_libraries_info, cc.cc_th_libraries, for_th_only = True),
        args,
    )

    return struct(
        args = args,
        compile_flags = compile_flags,
        inputs = depset(transitive = [
            source_files,
            boot_files,
            extra_source_files,
            depset(cc.hdrs),
            dep_info.package_databases,
            dep_info.interface_dirs,
            th_dep_info.package_databases,
            th_dep_info.interface_dirs,
            th_dep_info.hs_libraries,
            plugin_dep_info.package_databases,
            plugin_dep_info.interface_dirs,
            plugin_dep_info.hs_libraries,
            depset(get_ghci_library_files(hs, cc.cc_libraries_info, cc.transitive_th_libraries + cc.plugin_libraries)),
            java.inputs,
            preprocessors.inputs,
            plugin_tool_inputs,
        ]),
        input_manifests = preprocessors.input_manifests + plugin_tool_input_manifests,
        object_files = object_files,
        dyn_object_files = dyn_object_files,
        interface_files = interface_files,
        outputs = object_files + dyn_object_files + interface_files,
        source_files = source_files,
        boot_files = boot_files,
        extra_source_files = extra_source_files,
        import_dirs = import_dirs,
        env = dicts.add(
            java.env,
            hs.env,
            cc.env,
        ),
    )

def _hpc_compiler_args(hs):
    hpcdir = "{}/{}/{}_.hpc".format(hs.bin_dir.path, hs.package_root, hs.name)
    return ["-fhpc", "-hpcdir", hpcdir]

def _coverage_datum(mix_file, src_file, target_label):
    return struct(
        mix_file = mix_file,
        src_file = src_file,
        target_label = target_label,
    )

def compile_binary(
        hs,
        cc,
        java,
        posix,
        dep_info,
        th_dep_info,
        plugin_dep_info,
        srcs,
        module_map,
        import_dir_map,
        extra_srcs,
        user_compile_flags,
        dynamic,
        with_profiling,
        interfaces_dir,
        objects_dir,
        main_function,
        version,
        inspect_coverage = False,
        plugins = [],
        non_default_plugins = [],
        preprocessors = []):
    """Compile a Haskell target into object files suitable for linking.

    Returns:
      struct with the following fields:
        object_files: list of static object files
        dyn_object_files: list of dynamic object files
        modules: set of module names
        source_files: set of Haskell source files
        boot_files: set of Haskell boot files
    """
    c = _compilation_defaults(
        hs,
        cc,
        java,
        posix,
        dep_info,
        th_dep_info,
        plugin_dep_info,
        srcs,
        module_map,
        import_dir_map,
        extra_srcs,
        user_compile_flags,
        "dynamic" if dynamic else "static",
        with_profiling,
        interfaces_dir,
        objects_dir,
        my_pkg_id = None,
        version = version,
        plugins = plugins,
        non_default_plugins = non_default_plugins,
        preprocessors = preprocessors,
    )
    c.args.add_all(["-main-is", main_function])
    if dynamic:
        # For binaries, GHC creates .o files even for code to be
        # linked dynamically. So we have to force the object suffix to
        # be consistent with the dynamic object suffix in the library
        # case.
        c.args.add_all(["-dynamic", "-osuf dyn_o"])

    coverage_data = []
    if inspect_coverage:
        c.args.add_all(_hpc_compiler_args(hs))
        for (module, info) in module_map.items():
            mix_file = hs.actions.declare_file("{name}_.hpc/{module}.mix".format(name = hs.name, module = module))
            coverage_data.append(_coverage_datum(mix_file, info.src, hs.label))

    if srcs:
        hs.toolchain.actions.run_ghc(
            hs,
            cc,
            inputs = c.inputs,
            input_manifests = c.input_manifests,
            outputs = c.outputs + [datum.mix_file for datum in coverage_data],
            mnemonic = "HaskellBuildBinary" + ("Prof" if with_profiling else ""),
            progress_message = "HaskellBuildBinary {}".format(hs.label),
            env = c.env,
            arguments = c.args,
        )

    return struct(
        object_files = c.object_files,
        dyn_object_files = c.dyn_object_files,
        source_files = c.source_files,
        boot_files = c.boot_files,
        extra_source_files = c.extra_source_files,
        import_dirs = c.import_dirs,
        compile_flags = c.compile_flags,
        coverage_data = coverage_data,
    )

def compile_library(
        hs,
        cc,
        java,
        posix,
        dep_info,
        th_dep_info,
        plugin_dep_info,
        srcs,
        module_map,
        import_dir_map,
        extra_srcs,
        user_compile_flags,
        with_shared,
        with_profiling,
        interfaces_dir,
        objects_dir,
        my_pkg_id,
        plugins = [],
        non_default_plugins = [],
        preprocessors = []):
    """Build arguments for Haskell package build.

    Returns:
      struct with the following fields:
        interfaces_dir: directory containing interface files
        interface_files: list of interface files
        object_files: list of static object files
        dyn_object_files: list of dynamic object files
        compile_flags: list of string arguments suitable for Haddock
        modules: set of module names
        source_files: set of Haskell module files
        boot_files: set of Haskell boot files
        import_dirs: import directories that should make all modules visible (for GHCi)
    """
    c = _compilation_defaults(
        hs,
        cc,
        java,
        posix,
        dep_info,
        th_dep_info,
        plugin_dep_info,
        srcs,
        module_map,
        import_dir_map,
        extra_srcs,
        user_compile_flags,
        "dynamic-too" if with_shared else "static",
        with_profiling,
        interfaces_dir,
        objects_dir,
        my_pkg_id = my_pkg_id,
        version = my_pkg_id.version,
        plugins = plugins,
        non_default_plugins = non_default_plugins,
        preprocessors = preprocessors,
    )
    if with_shared:
        c.args.add("-dynamic-too")

        # See Note [No PIE when linking] in haskell/private/actions/link.bzl
        if not hs.toolchain.is_darwin and not hs.toolchain.is_windows:
            if hs.toolchain.numeric_version < [8, 10]:
                c.args.add("-optl-no-pie")

    coverage_data = []
    if hs.coverage_enabled:
        c.args.add_all(_hpc_compiler_args(hs))
        pkg_id_string = pkg_id.to_string(my_pkg_id)
        for (module, info) in module_map.items():
            mix_file = hs.actions.declare_file("{name}_.hpc/{pkg}/{module}.mix".format(name = hs.name, pkg = pkg_id_string, module = module))
            coverage_data.append(_coverage_datum(mix_file, info.src, hs.label))

    if srcs:
        hs.toolchain.actions.run_ghc(
            hs,
            cc,
            inputs = c.inputs,
            input_manifests = c.input_manifests,
            outputs = c.outputs + [datum.mix_file for datum in coverage_data],
            mnemonic = "HaskellBuildLibrary" + ("Prof" if with_profiling else ""),
            progress_message = "HaskellBuildLibrary {}".format(hs.label),
            env = c.env,
            arguments = c.args,
        )

    return struct(
        interface_files = c.interface_files,
        object_files = c.object_files,
        dyn_object_files = c.dyn_object_files,
        compile_flags = c.compile_flags,
        source_files = c.source_files,
        boot_files = c.boot_files,
        extra_source_files = c.extra_source_files,
        import_dirs = c.import_dirs,
        coverage_data = coverage_data,
    )
