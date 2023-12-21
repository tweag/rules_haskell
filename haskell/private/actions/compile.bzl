"""Actions for compiling Haskell source code"""

load(
    ":private/packages.bzl",
    "expose_packages",
    "pkg_info_to_compile_flags",
)
load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
load(":private/pkg_id.bzl", "pkg_id")
load(
    ":providers.bzl",
    "all_dependencies_package_ids",
)
load(
    ":private/cc_libraries.bzl",
    "get_ghci_library_files",
    "link_libraries",
)
load("@bazel_skylib//lib:sets.bzl", "sets")
load(
    ":private/actions/process_hsc_file.bzl",
    "preprocess_hsc_flags_and_inputs",
    "process_hsc_file",
)

def _compilation_defaults(
        hs,
        cc,
        java,
        dep_info,
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
        extra_ldflags_file,
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

    if hs.toolchain.is_darwin:
        # assume `otool` and `install_name_tool` are available at the same location as `ar`
        ar_bindir = paths.dirname(cc.tools.ar)

        compile_flags += [
            "-pgmotool=" + paths.join(ar_bindir, "otool"),
            "-pgminstall_name_tool=" + paths.join(ar_bindir, "install_name_tool"),
        ]

    package_ids = []
    all_plugins = plugins + non_default_plugins
    for plugin in all_plugins:
        package_ids.extend(all_dependencies_package_ids(plugin.deps))

    (pkg_info_inputs, pkg_info_args) = pkg_info_to_compile_flags(
        hs,
        pkg_info = expose_packages(
            package_ids = hs.package_ids,
            package_databases = dep_info.package_databases,
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
    hsc_flags, hsc_inputs = preprocess_hsc_flags_and_inputs(dep_info, user_compile_flags, version)

    # Add import hierarchy root.
    # Note that this is not perfect, since GHC requires hs-boot files
    # to be in the same directory as the corresponding .hs file.  Thus
    # the two must both have the same root; i.e., both plain files,
    # both in bin_dir, or both in genfiles_dir.

    import_dirs = sets.make([
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
            s0, idir = process_hsc_file(hs, cc, hsc_flags, hsc_inputs, s)
            source_files.append(s0)
            sets.insert(import_dirs, idir)
        elif s.extension in ["hs-boot", "lhs-boot"]:
            boot_files.append(s)
        else:
            source_files.append(s)

        if s in import_dir_map:
            idir = import_dir_map[s]
            sets.insert(import_dirs, idir)

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
        get_ghci_library_files(hs, cc.cc_libraries_info, cc.cc_libraries, for_th_only = True),
        args,
    )

    input_files = []
    if extra_ldflags_file:
        args.add("-optl@{}".format(extra_ldflags_file.path))
        input_files.append(extra_ldflags_file)

    return struct(
        args = args,
        compile_flags = compile_flags,
        inputs = depset(input_files, transitive = [
            source_files,
            boot_files,
            extra_source_files,
            depset(cc.hdrs),
            dep_info.package_databases,
            dep_info.interface_dirs,
            dep_info.hs_libraries,
            plugin_dep_info.package_databases,
            plugin_dep_info.interface_dirs,
            plugin_dep_info.hs_libraries,
            depset(get_ghci_library_files(hs, cc.cc_libraries_info, cc.transitive_libraries + cc.plugin_libraries)),
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
        posix,  # @unused
        dep_info,
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
        extra_ldflags_file,
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
        dep_info,
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
        extra_ldflags_file = extra_ldflags_file,
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
        posix,  # @unused
        dep_info,
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
        extra_ldflags_file,
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
        dep_info,
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
        extra_ldflags_file = extra_ldflags_file,
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
