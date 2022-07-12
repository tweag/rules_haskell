"""Implementation of core Haskell rules"""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load(
    ":providers.bzl",
    "C2hsLibraryInfo",
    "HaddockInfo",
    "HaskellInfo",
    "HaskellLibraryInfo",
    "HaskellToolchainLibraryInfo",
    "all_dependencies_package_ids",
)
load(":cc.bzl", "cc_interop_info")
load(
    ":private/actions/info.bzl",
    "compile_info_output_groups",
    "library_info_output_groups",
)
load(
    ":private/actions/link.bzl",
    "darwin_flags_for_linking_indirect_cc_deps",
    "dynamic_library_filename",
    "link_binary",
    "link_library_dynamic",
    "link_library_static",
)
load(":private/actions/package.bzl", "package")
load(":private/plugins.bzl", "resolve_plugin_tools")
load(":private/actions/runghc.bzl", "build_haskell_runghc")
load(":private/context.bzl", "haskell_context")
load(":private/dependencies.bzl", "gather_dep_info")
load(":private/expansions.bzl", "haskell_library_expand_make_variables")
load(":private/java.bzl", "java_interop_info")
load(":private/mode.bzl", "is_profiling_enabled")
load(
    ":private/path_utils.bzl",
    "determine_module_names",
    "get_dynamic_hs_lib_name",
    "get_lib_extension",
    "get_static_hs_lib_name",
    "infer_main_module",
    "ln",
    "match_label",
    "parse_pattern",
)
load(":private/pkg_id.bzl", "pkg_id")
load(":private/set.bzl", "set")
load(":private/list.bzl", "list")
load(":private/version_macros.bzl", "generate_version_macros")
load(":providers.bzl", "GhcPluginInfo", "HaskellCoverageInfo")
load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_skylib//lib:collections.bzl", "collections")
load("@bazel_skylib//lib:shell.bzl", "shell")
load("@rules_cc//cc:find_cc_toolchain.bzl", "find_cc_toolchain")
load("//haskell/experimental:providers.bzl", "HaskellModuleInfo")
load("//haskell/experimental/private:module.bzl", "build_haskell_modules", "get_module_path_from_target")

# Note [Empty Libraries]
#
# GHC 8.10.x wants to load the shared libraries corresponding to packages needed
# for running TemplateHaskell splices. It wants to do this even when all the
# necessary object files are passed in the command line.
#
# In order to satisfy GHC, and yet avoid passing the linked library as input, we
# create a ficticious package which points to an empty shared library. The
# ficticious and the real package share the same interface files.
#
# Avoiding to pass the real shared library as input is necessary when building
# individual modules with haskell_module, otherwise building the module would
# need to wait until all of the modules of library dependencies have been built.
#
# See Note [Narrowed Dependencies] for an overview of what this feature is
# needed for.

def _prepare_srcs(srcs):
    srcs_files = []
    import_dir_map = {}

    for src in srcs:
        # If it has the "files" attribute, it must be a Target
        if hasattr(src, "files"):
            if C2hsLibraryInfo in src:
                srcs_files += src.files.to_list()
                for f in src.files.to_list():
                    import_dir_map[f] = src[C2hsLibraryInfo].import_dir
            else:
                srcs_files += src.files.to_list()

            # otherwise it's just a file

        else:
            srcs_files.append(src)

    return srcs_files, import_dir_map

def haskell_test_impl(ctx):
    return _haskell_binary_common_impl(ctx, is_test = True)

def haskell_binary_impl(ctx):
    return _haskell_binary_common_impl(ctx, is_test = False)

def _should_inspect_coverage(ctx, hs, is_test):
    return hs.coverage_enabled and is_test

def _coverage_enabled_for_target(coverage_source_patterns, label):
    for pat in coverage_source_patterns:
        if match_label(pat, label):
            return True

    return False

# Mix files refer to genfile srcs including their root. Therefore, we
# must condition the src filepaths passed in for coverage to match.
def _condition_coverage_src(hs, src):
    if not src.path.startswith(hs.genfiles_dir.path):
        return src

    """ Genfiles have the genfile directory as part of their path,
    so declaring a file with the sample path actually makes the new
    file double-qualified by the genfile directory.

    This is necessary because mix files capture the genfile
    path before compilation, and then expect those files to be
    qualified by the genfile directory when `hpc report` or
    `hpc markup` are used. But, genfiles included as runfiles
    are no longer qualified. So, double-qualifying them results in
    only one level of qualification as runfiles.
    """
    conditioned_src = hs.actions.declare_file(src.path)
    hs.actions.run_shell(
        inputs = [src],
        outputs = [conditioned_src],
        arguments = [
            src.path,
            conditioned_src.path,
        ],
        command = """
        mkdir -p $(dirname "$2") && cp "$1" "$2"
        """,
    )

    return conditioned_src

def _resolve_preprocessors(ctx, preprocessors):
    if not hasattr(ctx, "resolve_tools"):
        # No resolve_tools when ctx is faked (see protobuf.bzl).
        return struct(
            inputs = depset(),
            input_manifests = [],
        )
    (inputs, input_manifests) = ctx.resolve_tools(tools = preprocessors)
    return struct(
        inputs = inputs,
        input_manifests = input_manifests,
    )

def haskell_module_from_target(m):
    """ Produces the module name from a HaskellModuleInfo """
    return paths.split_extension(get_module_path_from_target(m))[0].replace("/", ".")

def is_main_as_haskell_module(modules, main_function):
    main_module = infer_main_module(main_function).replace(".", "/")
    for m in modules:
        if haskell_module_from_target(m) == main_module:
            return True
    return False

def _haskell_binary_common_impl(ctx, is_test):
    hs = haskell_context(ctx)
    deps = ctx.attr.deps + ctx.attr.narrowed_deps
    dep_info = gather_dep_info(ctx.attr.name, ctx.attr.deps)
    all_deps_info = gather_dep_info(ctx.attr.name, deps)

    modules = ctx.attr.modules
    if modules and ctx.files.srcs:
        fail("""Only one of "srcs" or "modules" attributes must be specified in {}""".format(ctx.label))

    if not modules and ctx.attr.narrowed_deps:
        fail("""The attribute "narrowed_deps" can only be used if "modules" is specified in {}""".format(ctx.label))

    # Note [Plugin order]
    plugin_decl = reversed(ctx.attr.plugins)
    non_default_plugin_decl = reversed(ctx.attr.non_default_plugins)
    all_plugin_decls = plugin_decl + non_default_plugin_decl

    plugin_dep_info = gather_dep_info(
        ctx.attr.name,
        [dep for plugin in all_plugin_decls for dep in plugin[GhcPluginInfo].deps],
    )

    # Add any interop info for other languages.
    cc = cc_interop_info(
        ctx,
        override_cc_toolchain = hs.tools_config.maybe_exec_cc_toolchain,
    )
    java = java_interop_info(deps)

    # Make shell tools available.
    posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]

    # Determine file directories.
    interfaces_dir = paths.join("_iface", hs.name)
    objects_dir = paths.join("_obj", hs.name)

    with_profiling = is_profiling_enabled(hs)
    srcs_files, import_dir_map = _prepare_srcs(ctx.attr.srcs)
    main_as_haskell_module = is_main_as_haskell_module(modules, ctx.attr.main_function)
    module_map = determine_module_names(srcs_files, not main_as_haskell_module, ctx.attr.main_function, ctx.file.main_file)
    inspect_coverage = _should_inspect_coverage(ctx, hs, is_test)

    dynamic = not ctx.attr.linkstatic
    if with_profiling or hs.toolchain.static_runtime:
        # NOTE We can't have profiling and dynamic code at the
        # same time, see:
        # https://ghc.haskell.org/trac/ghc/ticket/15394
        # Also, static GHC doesn't support dynamic code
        dynamic = False

    extra_ldflags_file = darwin_flags_for_linking_indirect_cc_deps(hs, cc, posix, hs.name, dynamic)

    module_outputs = build_haskell_modules(
        ctx,
        hs,
        cc,
        posix,
        "",
        with_profiling,
        dynamic,
        extra_ldflags_file,
        interfaces_dir,
        objects_dir,
    )

    plugins = [resolve_plugin_tools(ctx, plugin[GhcPluginInfo]) for plugin in plugin_decl]
    non_default_plugins = [resolve_plugin_tools(ctx, plugin[GhcPluginInfo]) for plugin in non_default_plugin_decl]
    preprocessors = _resolve_preprocessors(ctx, ctx.attr.tools)
    user_compile_flags = haskell_library_expand_make_variables("ghcopts", ctx, ctx.attr.ghcopts)
    c = hs.toolchain.actions.compile_binary(
        hs,
        cc,
        java,
        posix,
        dep_info,
        plugin_dep_info,
        srcs = srcs_files,
        module_map = module_map,
        import_dir_map = import_dir_map,
        extra_srcs = depset(ctx.files.extra_srcs),
        user_compile_flags = user_compile_flags,
        dynamic = dynamic,
        with_profiling = with_profiling,
        interfaces_dir = interfaces_dir,
        objects_dir = objects_dir,
        main_function = ctx.attr.main_function,
        version = ctx.attr.version,
        inspect_coverage = inspect_coverage,
        extra_ldflags_file = extra_ldflags_file,
        plugins = plugins,
        non_default_plugins = non_default_plugins,
        preprocessors = preprocessors,
    )

    # gather intermediary code coverage instrumentation data
    coverage_data = c.coverage_data
    for dep in deps:
        if HaskellCoverageInfo in dep:
            coverage_data += dep[HaskellCoverageInfo].coverage_data
            coverage_data = list.dedup_on(_get_mix_filepath, coverage_data)

    user_compile_flags = haskell_library_expand_make_variables("ghcopts", ctx, ctx.attr.ghcopts)
    (binary, solibs) = link_binary(
        hs,
        cc,
        posix,
        all_deps_info,
        ctx.files.extra_srcs,
        user_compile_flags,
        c.object_files + c.dyn_object_files,
        module_outputs.os,
        extra_ldflags_file,
        dynamic = dynamic,
        with_profiling = with_profiling,
        version = ctx.attr.version,
    )

    hs_info = HaskellInfo(
        package_databases = all_deps_info.package_databases,
        version_macros = set.empty(),
        source_files = depset(transitive = [c.source_files, module_outputs.repl_info.source_files]),
        boot_files = depset(transitive = [c.boot_files, module_outputs.repl_info.boot_files]),
        extra_source_files = c.extra_source_files,
        import_dirs = set.mutable_union(c.import_dirs, module_outputs.repl_info.import_dirs),
        hs_libraries = all_deps_info.hs_libraries,
        deps_hs_libraries = all_deps_info.deps_hs_libraries,
        interface_dirs = all_deps_info.interface_dirs,
        deps_interface_dirs = all_deps_info.deps_interface_dirs,
        compile_flags = c.compile_flags,
        user_compile_flags = user_compile_flags + module_outputs.repl_info.user_compile_flags,
        user_repl_flags = haskell_library_expand_make_variables("repl_ghci_args", ctx, ctx.attr.repl_ghci_args),
    )
    cc_info = cc_common.merge_cc_infos(
        cc_infos = [dep[CcInfo] for dep in deps if CcInfo in dep],
    )

    target_files = depset([binary])

    user_compile_flags = haskell_library_expand_make_variables("ghcopts", ctx, ctx.attr.ghcopts)
    extra_args = haskell_library_expand_make_variables("runcompile_flags", ctx, ctx.attr.runcompile_flags)
    build_haskell_runghc(
        hs,
        cc,
        posix,
        runghc_wrapper = ctx.file._ghci_repl_wrapper,
        extra_args = extra_args,
        user_compile_flags = user_compile_flags,
        output = ctx.outputs.runghc,
        package_databases = all_deps_info.package_databases,
        version = ctx.attr.version,
        hs_info = hs_info,
    )

    executable = binary
    extra_runfiles = []

    if inspect_coverage:
        binary_path = paths.join(ctx.workspace_name, binary.short_path)
        hpc_path = paths.join(ctx.workspace_name, hs.toolchain.tools.hpc.short_path)
        tix_file_path = hs.label.name + ".tix"
        mix_file_paths = [
            paths.join(ctx.workspace_name, datum.mix_file.short_path)
            for datum in coverage_data
        ]
        mix_file_paths = collections.uniq(mix_file_paths)  # remove duplicates

        # find which modules to exclude from coverage analysis, by using the specified source patterns
        raw_coverage_source_patterns = ctx.attr.experimental_coverage_source_patterns
        coverage_source_patterns = [parse_pattern(ctx, pat) for pat in raw_coverage_source_patterns]
        modules_to_exclude = [paths.split_extension(datum.mix_file.basename)[0] for datum in coverage_data if not _coverage_enabled_for_target(coverage_source_patterns, datum.target_label)]
        modules_to_exclude = collections.uniq(modules_to_exclude)  # remove duplicates

        expected_covered_expressions_percentage = ctx.attr.expected_covered_expressions_percentage
        expected_uncovered_expression_count = ctx.attr.expected_uncovered_expression_count
        strict_coverage_analysis = ctx.attr.strict_coverage_analysis
        coverage_report_format = ctx.attr.coverage_report_format

        if coverage_report_format != "text" and coverage_report_format != "html":
            fail("""haskell_test attribute "coverage_report_format" must be one of "text" or "html".""")

        wrapper = hs.actions.declare_file("{}_coverage/coverage_wrapper.sh".format(ctx.label.name))
        ctx.actions.expand_template(
            template = ctx.file._coverage_wrapper_template,
            output = wrapper,
            substitutions = {
                "{binary_path}": shell.quote(binary_path),
                "{hpc_path}": shell.quote(hpc_path),
                "{tix_file_path}": shell.quote(tix_file_path),
                "{expected_covered_expressions_percentage}": shell.quote(str(expected_covered_expressions_percentage)),
                "{expected_uncovered_expression_count}": shell.quote(str(expected_uncovered_expression_count)),
                "{mix_file_paths}": shell.array_literal(mix_file_paths),
                "{modules_to_exclude}": shell.array_literal(modules_to_exclude),
                "{strict_coverage_analysis}": str(strict_coverage_analysis),
                "{coverage_report_format}": shell.quote(ctx.attr.coverage_report_format),
                "{package_path}": shell.quote(ctx.label.package),
            },
            is_executable = True,
        )
        executable = wrapper
        mix_runfiles = [datum.mix_file for datum in coverage_data]
        srcs_runfiles = [_condition_coverage_src(hs, datum.src_file) for datum in coverage_data]
        extra_runfiles = [
            ctx.file._bash_runfiles,
            hs.toolchain.tools.hpc,
            binary,
        ] + mix_runfiles + srcs_runfiles + java.inputs.to_list()

    return [
        hs_info,
        cc_info,
        DefaultInfo(
            executable = executable,
            files = target_files,
            runfiles = ctx.runfiles(
                files = extra_runfiles + solibs,
                collect_data = True,
            ),
        ),
        OutputGroupInfo(**compile_info_output_groups(
            name = ctx.label.name,
            workspace_name = ctx.workspace_name,
            hs = hs,
            cc = cc,
            c = c,
            posix = posix,
            runfiles = ctx.runfiles(collect_data = True).files,
        )),
    ]

def _create_empty_library(hs, cc, posix, my_pkg_id, with_shared, with_profiling, empty_libs_dir):
    """See Note [Empty Libraries]"""
    dep_info = gather_dep_info("haskell_module-empty_lib", [])
    empty_c = hs.actions.declare_file("empty.c")
    hs.actions.write(empty_c, "")

    static_library = link_library_static(
        hs,
        cc,
        posix,
        dep_info,
        depset([empty_c]),
        my_pkg_id,
        with_profiling = with_profiling,
        libdir = empty_libs_dir,
    )
    libs = [static_library]

    if with_shared:
        dynamic_library = link_library_dynamic(
            hs,
            cc,
            posix,
            dep_info,
            depset(),
            depset([empty_c]),
            my_pkg_id,
            [],
            None,
            empty_libs_dir,
        )
        libs = [dynamic_library, static_library]

    return libs

def haskell_library_impl(ctx):
    hs = haskell_context(ctx)
    deps = ctx.attr.deps + ctx.attr.exports + ctx.attr.narrowed_deps
    dep_info = gather_dep_info(ctx.attr.name, ctx.attr.deps + ctx.attr.exports)
    narrowed_deps_info = gather_dep_info(ctx.attr.name, ctx.attr.narrowed_deps)
    all_deps_info = gather_dep_info(ctx.attr.name, deps)
    all_plugins = ctx.attr.plugins + ctx.attr.non_default_plugins
    plugin_dep_info = gather_dep_info(
        ctx.attr.name,
        [dep for plugin in all_plugins for dep in plugin[GhcPluginInfo].deps],
    )

    modules = ctx.attr.modules
    if modules and ctx.files.srcs:
        fail("""Only one of "srcs" or "modules" attributes must be specified in {}""".format(ctx.label))

    if not modules and ctx.attr.narrowed_deps:
        fail("""The attribute "narrowed_deps" is enabled only if "modules" is specified in {}""".format(ctx.label))

    # Add any interop info for other languages.
    cc = cc_interop_info(
        ctx,
        override_cc_toolchain = hs.tools_config.maybe_exec_cc_toolchain,
    )
    java = java_interop_info(ctx.attr.deps + ctx.attr.narrowed_deps)

    # Make shell tools available.
    posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]

    with_profiling = is_profiling_enabled(hs)
    srcs_files, import_dir_map = _prepare_srcs(ctx.attr.srcs)
    module_map = determine_module_names(srcs_files)

    package_name = getattr(ctx.attr, "package_name", None)
    version = getattr(ctx.attr, "version", None)
    my_pkg_id = pkg_id.new(ctx.label, package_name, version)

    # If we're compiling a package, put the interfaces inside the
    # package directory.
    interfaces_dir = paths.join(pkg_id.to_string(my_pkg_id), "_iface")
    objects_dir = paths.join("_obj", hs.name)

    non_empty = srcs_files or modules

    with_shared = not ctx.attr.linkstatic
    if with_profiling or hs.toolchain.static_runtime:
        # NOTE We can't have profiling and dynamic code at the
        # same time, see:
        # https://ghc.haskell.org/trac/ghc/ticket/15394
        # Also, static GHC doesn't support dynamic code
        with_shared = False

    extra_ldflags_file = darwin_flags_for_linking_indirect_cc_deps(hs, cc, posix, dynamic_library_filename(hs, my_pkg_id), with_shared)

    module_outputs = build_haskell_modules(
        ctx,
        hs,
        cc,
        posix,
        pkg_id.to_string(my_pkg_id),
        with_profiling,
        with_shared,
        extra_ldflags_file,
        interfaces_dir,
        objects_dir,
    )

    plugins = [resolve_plugin_tools(ctx, plugin[GhcPluginInfo]) for plugin in ctx.attr.plugins]
    non_default_plugins = [resolve_plugin_tools(ctx, plugin[GhcPluginInfo]) for plugin in ctx.attr.non_default_plugins]
    preprocessors = _resolve_preprocessors(ctx, ctx.attr.tools)
    user_compile_flags = haskell_library_expand_make_variables("ghcopts", ctx, ctx.attr.ghcopts)
    c = hs.toolchain.actions.compile_library(
        hs,
        cc,
        java,
        posix,
        dep_info,
        plugin_dep_info,
        srcs = srcs_files,
        module_map = module_map,
        import_dir_map = import_dir_map,
        extra_srcs = depset(ctx.files.extra_srcs),
        user_compile_flags = user_compile_flags,
        with_shared = with_shared,
        with_profiling = with_profiling,
        interfaces_dir = interfaces_dir,
        objects_dir = objects_dir,
        my_pkg_id = my_pkg_id,
        extra_ldflags_file = extra_ldflags_file,
        plugins = plugins,
        non_default_plugins = non_default_plugins,
        preprocessors = preprocessors,
    )

    other_modules = ctx.attr.hidden_modules
    exposed_modules_reexports = _exposed_modules_reexports(ctx.attr.reexported_modules)
    haskell_module_names = [haskell_module_from_target(m) for m in modules]
    exposed_modules = set.from_list(module_map.keys() + exposed_modules_reexports + haskell_module_names)
    set.mutable_difference(exposed_modules, set.from_list(other_modules))
    exposed_modules = set.to_list(exposed_modules)

    if non_empty:
        static_library = link_library_static(
            hs,
            cc,
            posix,
            all_deps_info,
            depset(c.object_files, transitive = [module_outputs.os]),
            my_pkg_id,
            with_profiling = with_profiling,
        )
    else:
        static_library = None

    if with_shared and non_empty:
        dynamic_library = link_library_dynamic(
            hs,
            cc,
            posix,
            all_deps_info,
            depset(ctx.files.extra_srcs),
            depset(c.dyn_object_files, transitive = [module_outputs.dyn_os]),
            my_pkg_id,
            user_compile_flags,
            extra_ldflags_file,
        )
    else:
        dynamic_library = None

    conf_file, cache_file = package(
        hs,
        cc,
        posix,
        all_deps_info,
        with_shared,
        exposed_modules,
        other_modules,
        my_pkg_id,
        non_empty,
    )

    empty_libs_dir = "empty_libs"
    conf_file_empty, cache_file_empty = package(
        hs,
        cc,
        posix,
        all_deps_info,
        with_shared,
        exposed_modules,
        other_modules,
        my_pkg_id,
        non_empty,
        empty_libs_dir,
    )

    interface_dirs = depset(
        direct = c.interface_files,
        transitive = [all_deps_info.interface_dirs, module_outputs.abis, module_outputs.his, module_outputs.dyn_his],
    )

    version_macros = set.empty()
    if version:
        package_name = hs.name
        if hasattr(ctx.attr, "package_name") and ctx.attr.package_name:
            package_name = ctx.attr.package_name
        version_macros = set.singleton(
            generate_version_macros(ctx, package_name, version),
        )

    empty_libs = _create_empty_library(hs, cc, posix, my_pkg_id, with_shared, with_profiling, empty_libs_dir)

    export_infos = gather_dep_info(ctx.attr.name, ctx.attr.exports)
    hs_info = HaskellInfo(
        package_databases = depset([cache_file], transitive = [all_deps_info.package_databases]),
        empty_lib_package_databases = depset(
            direct = [cache_file_empty],
            transitive = [
                # Mind the order in which databases are specified here.
                # See Note [Deps as both narrowed and not narrowed].
                narrowed_deps_info.empty_lib_package_databases,
                export_infos.empty_lib_package_databases,
                dep_info.package_databases,
            ],
            order = "preorder",
        ),
        version_macros = version_macros,
        source_files = depset(transitive = [c.source_files, module_outputs.repl_info.source_files]),
        boot_files = depset(transitive = [c.boot_files, module_outputs.repl_info.boot_files]),
        extra_source_files = c.extra_source_files,
        import_dirs = set.mutable_union(c.import_dirs, set.mutable_union(export_infos.import_dirs, module_outputs.repl_info.import_dirs)),
        hs_libraries = depset(
            direct = [lib for lib in [static_library, dynamic_library] if lib],
            transitive = [all_deps_info.hs_libraries],
        ),
        deps_hs_libraries = depset(
            transitive = [dep_info.hs_libraries, narrowed_deps_info.deps_hs_libraries],
        ),
        empty_hs_libraries = depset(
            direct = empty_libs,
            transitive = [all_deps_info.empty_hs_libraries, export_infos.empty_hs_libraries],
        ),
        interface_dirs = depset(transitive = [interface_dirs, export_infos.interface_dirs]),
        deps_interface_dirs = depset(transitive = [dep_info.interface_dirs, narrowed_deps_info.deps_interface_dirs]),
        compile_flags = c.compile_flags,
        user_compile_flags = user_compile_flags + module_outputs.repl_info.user_compile_flags,
        user_repl_flags = haskell_library_expand_make_variables("repl_ghci_args", ctx, ctx.attr.repl_ghci_args),
        per_module_transitive_interfaces = module_outputs.per_module_transitive_interfaces,
        per_module_transitive_objects = module_outputs.per_module_transitive_objects,
        per_module_transitive_dyn_objects = module_outputs.per_module_transitive_dyn_objects,
        per_module_transitive_abis = module_outputs.per_module_transitive_abis,
    )

    exports = [
        reexp[HaskellLibraryInfo]
        for reexp in ctx.attr.exports
        if HaskellCoverageInfo in reexp
    ]
    lib_info = HaskellLibraryInfo(
        package_id = pkg_id.to_string(my_pkg_id),
        version = version,
        exports = exports,
    )

    dep_coverage_data = []
    for dep in deps:
        if HaskellCoverageInfo in dep:
            dep_coverage_data += dep[HaskellCoverageInfo].coverage_data

    coverage_data = dep_coverage_data + c.coverage_data
    coverage_data = list.dedup_on(_get_mix_filepath, coverage_data)

    coverage_info = HaskellCoverageInfo(
        coverage_data = coverage_data,
    )

    target_files = depset([file for file in [static_library, dynamic_library] if file])

    if hasattr(ctx, "outputs"):
        extra_args = haskell_library_expand_make_variables("runcompile_flags", ctx, ctx.attr.runcompile_flags)
        user_compile_flags = haskell_library_expand_make_variables("ghcopts", ctx, ctx.attr.ghcopts)
        build_haskell_runghc(
            hs,
            cc,
            posix,
            runghc_wrapper = ctx.file._ghci_repl_wrapper,
            extra_args = extra_args,
            user_compile_flags = user_compile_flags,
            output = ctx.outputs.runghc,
            package_databases = all_deps_info.package_databases,
            version = ctx.attr.version,
            hs_info = hs_info,
            lib_info = lib_info,
        )

    default_info = None

    if hasattr(ctx, "runfiles"):
        default_info = DefaultInfo(
            files = target_files,
            runfiles = ctx.runfiles(transitive_files = java.inputs, collect_data = True),
        )
    else:
        default_info = DefaultInfo(
            files = target_files,
        )

    # Create a CcInfo provider so that CC rules can work with
    # a haskell library as if it was a regular CC one.

    # XXX: protobuf is passing a "patched ctx"
    # which includes the real ctx as "real_ctx"
    real_ctx = getattr(ctx, "real_ctx", ctx)
    cc_toolchain = find_cc_toolchain(real_ctx)
    feature_configuration = cc_common.configure_features(
        ctx = real_ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )
    if dynamic_library or static_library:
        linker_inputs = [
            cc_common.create_linker_input(
                owner = ctx.label,
                libraries = depset(direct = [
                    cc_common.create_library_to_link(
                        actions = ctx.actions,
                        feature_configuration = feature_configuration,
                        dynamic_library = dynamic_library,
                        dynamic_library_symlink_path = dynamic_library.basename if dynamic_library else "",
                        static_library = static_library,
                        cc_toolchain = cc_toolchain,
                    ),
                ]),
            ),
        ]
    else:
        linker_inputs = []
    compilation_context = cc_common.create_compilation_context()
    linking_context = cc_common.create_linking_context(
        linker_inputs = depset(direct = linker_inputs),
    )
    out_cc_info = cc_common.merge_cc_infos(
        cc_infos = [
            CcInfo(
                compilation_context = compilation_context,
                linking_context = linking_context,
            ),
        ] + [dep[CcInfo] for dep in deps if CcInfo in dep],
    )

    return [
        hs_info,
        out_cc_info,
        coverage_info,
        default_info,
        lib_info,
        OutputGroupInfo(**dicts.add(
            compile_info_output_groups(
                # For haskell_proto_aspect, which doesn't have a ctx.workspace_name,
                # just set it to "".  It won't matter in practice because those rules don't
                # have runfiles and won't be compiled directly anyway.
                workspace_name = getattr(ctx, "workspace_name", ""),
                hs = hs,
                cc = cc,
                name = ctx.label.name,
                c = c,
                posix = posix,
                runfiles = default_info.default_runfiles.files if getattr(default_info, "default_runfiles", None) else depset(),
            ),
            library_info_output_groups(
                name = ctx.label.name,
                hs = hs,
                hs_info = hs_info,
                lib_info = lib_info,
            ),
        )),
    ]

# We should not need this provider. It exists purely as a workaround
# for https://github.com/bazelbuild/bazel/issues/8129.
#
# TODO Get rid of this by computing a CcInfo in haskell_import
# instead. Currently blocked on upstream.
HaskellImportHack = provider()
HaskellToolchainLibraries = provider()

def haskell_toolchain_library_impl(ctx):
    hs = haskell_context(ctx)
    if ctx.attr.package:
        package = ctx.attr.package
    else:
        package = ctx.label.name

    libraries = ctx.attr._toolchain_libraries[HaskellToolchainLibraries].libraries
    target = libraries.get(package)

    if not target:
        fail(
            """
{} is not a toolchain library.
Check that it ships with your version of GHC.
The following toolchain libraries are available:
{}
            """.format(package, libraries),
        )

    return [
        target.default_info,
        target.hs_info,
        target.hs_lib_info,
        target.cc_info,
        target.haddock_info,
        HaskellToolchainLibraryInfo(),
        OutputGroupInfo(**library_info_output_groups(
            hs = hs,
            name = ctx.label.name,
            hs_info = target.hs_info,
            lib_info = target.hs_lib_info,
        )),
    ]

def _toolchain_library_symlink(dynamic_library):
    prefix = dynamic_library.owner.workspace_root.replace("_", "_U").replace("/", "_S")
    basename = dynamic_library.basename
    return paths.join(prefix, basename)

def haskell_toolchain_libraries_impl(ctx):
    hs = haskell_context(ctx)
    with_profiling = is_profiling_enabled(hs)
    with_threaded = "-threaded" in hs.toolchain.ghcopts

    cc_toolchain = find_cc_toolchain(ctx)
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )

    libraries = hs.toolchain.libraries

    # List of library in left-to-right post-ordering
    # Meaning, if package B depends on package A, then A will appear before B.
    ordered = depset(transitive = [
        target[HaskellImportHack].transitive_depends
        for target in hs.toolchain.libraries.values()
    ])

    library_dict = {}
    for package in ordered.to_list():
        target = libraries[package]

        # Construct CcInfo
        additional_link_inputs = []
        if with_profiling:
            # GHC does not provide dynamic profiling mode libraries. The dynamic
            # libraries that are available are missing profiling symbols, that
            # other profiling mode build results will reference. Therefore, we
            # don't import dynamic libraries in profiling mode.
            libs = {
                get_static_hs_lib_name(hs.toolchain.version, lib): {"static": lib}
                for lib in target[HaskellImportHack].static_profiling_libraries.to_list()
            }
        else:
            # Workaround for https://github.com/tweag/rules_haskell/issues/881
            # Static and dynamic libraries don't necessarily pair up 1 to 1.
            # E.g. the rts package in the Unix GHC bindist contains the
            # dynamic libHSrts and the static libCffi and libHSrts.
            libs = {}
            for lib in target[HaskellImportHack].dynamic_libraries.to_list():
                libname = get_dynamic_hs_lib_name(hs.toolchain.version, lib)
                if libname == "ffi" and libname in libs:
                    # Make sure that the file of libffi matching its soname
                    # ends up in target runfiles. Otherwise, execution will
                    # fail with "cannot open shared object file" errors.
                    # On Linux libffi comes in three shapes:
                    #   libffi.so, libffi.so.7, libffi.so.7.1.0
                    # (version numbers may vary)
                    # The soname is then libffi.so.7, meaning, at runtime the
                    # dynamic linker will look for libffi.so.7. So, that file
                    # should be the LibraryToLink.dynamic_library.
                    ext_components = get_lib_extension(lib).split(".")
                    if len(ext_components) == 2 and ext_components[0] == "so":
                        libs[libname]["dynamic"] = lib
                else:
                    libs[libname] = {"dynamic": lib}
            for lib in target[HaskellImportHack].static_libraries.to_list():
                name = get_static_hs_lib_name(with_profiling, lib)
                entry = libs.get(name, {})
                entry["static"] = lib
                libs[name] = entry

            # Avoid duplicate runtime and ffi libraries. These libraries come
            # in threaded and non-threaded flavors. Depending on the
            # compilation mode we want to forward only one or the other.
            # XXX: Threaded mode should be a per-target property. Use Bazel
            # build configurations and transitions to select the threaded or
            # non-threaded runtime and ffi on a per-target basis.
            if "HSrts_thr" in libs:
                if with_threaded:
                    libs["HSrts"] = libs["HSrts_thr"]
                libs.pop("HSrts_thr")
            if "Cffi_thr" in libs:
                if with_threaded:
                    libs["ffi"]["static"] = libs["Cffi_thr"]["static"]
                libs.pop("Cffi_thr")
        linker_inputs = [
            cc_common.create_linker_input(
                owner = ctx.label,
                libraries = depset(direct = [
                    cc_common.create_library_to_link(
                        actions = ctx.actions,
                        feature_configuration = feature_configuration,
                        dynamic_library = lib.get("dynamic", None),
                        dynamic_library_symlink_path =
                            _toolchain_library_symlink(lib["dynamic"]) if lib.get("dynamic") else "",
                        static_library = lib.get("static", None),
                        cc_toolchain = cc_toolchain,
                    )
                    for lib in libs.values()
                ]),
                user_link_flags = depset(direct = target[HaskellImportHack].linkopts),
            ),
        ]
        compilation_context = cc_common.create_compilation_context(
            headers = target[HaskellImportHack].headers,
            includes = target[HaskellImportHack].includes,
        )
        linking_context = cc_common.create_linking_context(
            linker_inputs = depset(direct = linker_inputs),
        )
        cc_info = CcInfo(
            compilation_context = compilation_context,
            linking_context = linking_context,
        )
        library_dict[package] = struct(
            default_info = target[DefaultInfo],
            hs_info = target[HaskellInfo],
            hs_lib_info = target[HaskellLibraryInfo],
            cc_info = cc_common.merge_cc_infos(cc_infos = [cc_info] + [
                library_dict[dep].cc_info
                for dep in target[HaskellImportHack].depends
            ]),
            haddock_info = target[HaddockInfo],
        )

    return [HaskellToolchainLibraries(libraries = library_dict)]

haskell_toolchain_libraries = rule(
    haskell_toolchain_libraries_impl,
    attrs = {
        "_cc_toolchain": attr.label(
            default = Label("@rules_cc//cc:current_cc_toolchain"),
        ),
    },
    toolchains = [
        "@rules_cc//cc:toolchain_type",
        "@rules_haskell//haskell:toolchain",
    ],
    fragments = ["cpp"],
)
"""Generate Haskell toolchain libraries.

This is an internal rule and should not be user facing.

This rule is a work-around for toolchain transitions not being implemented,
yet. See
https://github.com/bazelbuild/proposals/blob/master/designs/2019-02-12-toolchain-transitions.md
This will need to be revisited once that proposal is implemented.
"""

def haskell_import_impl(ctx):
    # The `allow_files` attribute of `rule` cannot define patterns of accepted
    # file extensions like `.so.*`. Instead, we check for the correct file
    # extensions here.
    for lib in ctx.files.shared_libraries:
        msg = "in shared_libraries attribute of haskell_import rule {}: " + \
              "source file '{}' is misplaced here " + \
              "(expected .dll, .dylib, .so or .so.*)"
        ext = get_lib_extension(lib)
        if not (ext in ["dll", "dylib", "so"] or ext.startswith("so.")):
            fail(msg.format(str(ctx.label), str(lib.short_path)))

    id = ctx.attr.id or ctx.attr.name
    target_files = [
        file
        for file in ctx.files.static_libraries + ctx.files.shared_libraries
    ]
    version_macros = set.empty()
    if ctx.attr.version != None:
        version_macros = set.singleton(
            generate_version_macros(ctx, ctx.label.name, ctx.attr.version),
        )
    hs_info = HaskellInfo(
        # XXX Empty set of conf and cache files only works for global db.
        package_databases = depset(),
        empty_lib_package_databases = depset(),
        version_macros = version_macros,
        source_files = depset(),
        boot_files = depset(),
        extra_source_files = depset(),
        import_dirs = set.empty(),
        hs_libraries = depset(),
        deps_hs_libraries = depset(),
        empty_hs_libraries = depset(),
        interface_dirs = depset(),
        deps_interface_dirs = depset(),
        compile_flags = [],
        user_compile_flags = [],
        user_repl_flags = [],
    )
    import_info = HaskellImportHack(
        # Make sure we're using the same order for dynamic_libraries,
        # static_libraries.
        dynamic_libraries = depset(ctx.files.shared_libraries),
        static_libraries = depset(ctx.files.static_libraries, order = "topological"),
        # NOTE: haskell_import is evaluated as a toolchain rule. Even if we
        # bazel build with -c dbg, this rule is still executed with
        # ctx.var["COMPILATION_MODE"] == "opt". Therefore, we need to carry
        # both profiling and non-profiling libraries forward so that a later
        # haskell_toolchain_library can select the appropriate artifacts.
        static_profiling_libraries = depset(ctx.files.static_profiling_libraries, order = "topological"),
        headers = depset(ctx.files.hdrs),
        includes = depset(ctx.attr.includes),
        linkopts = ctx.attr.linkopts,
        depends = [dep.label.name for dep in ctx.attr.deps],
        transitive_depends = depset(
            direct = [ctx.attr.name],
            transitive = [dep[HaskellImportHack].transitive_depends for dep in ctx.attr.deps],
            order = "postorder",
        ),
    )

    coverage_info = HaskellCoverageInfo(coverage_data = [])
    lib_info = HaskellLibraryInfo(
        package_id = id,
        version = ctx.attr.version,
        exports = [],
    )
    default_info = DefaultInfo(
        files = depset(target_files),
    )

    # This package haddock informations
    transitive_html = {id: ctx.file.haddock_html} if ctx.file.haddock_html else {}
    transitive_haddocks = {id: ctx.files.haddock_interfaces}

    # Add dependencies haddock informations
    for dep in ctx.attr.deps:
        transitive_html.update(dep[HaddockInfo].transitive_html)
        transitive_haddocks.update(dep[HaddockInfo].transitive_haddocks)

    haddock_info = HaddockInfo(
        package_id = id,
        transitive_html = transitive_html,
        transitive_haddocks = transitive_haddocks,
    )

    return [
        hs_info,
        import_info,
        coverage_info,
        default_info,
        lib_info,
        haddock_info,
    ]

def _exposed_modules_reexports(reexported_modules):
    """Creates a ghc-pkg-compatible list of reexport declarations.

    A ghc-pkg registration file declares reexports as part of the
    exposed-modules field in the following format:

    exposed-modules: A, B, C from pkg-c:C, D from pkg-d:Original.D

    Here, the Original.D module from pkg-d is renamed by virtue of a
    different name being used before the "from" keyword.

    This function creates a ghc-pkg-compatible list of reexport declarations
    (as shown above) from a dictionary mapping package targets to "Cabal-style"
    reexported-modules declarations. That is, something like:

    {
      ":pkg-c": "C",
      ":pkg-d": "Original.D as D",
      ":pkg-e": "E1, Original.E2 as E2",
    }

    Args:
      reexported_modules: a dictionary mapping package targets to "Cabal-style"
               reexported-modules declarations.

    Returns:
      a ghc-pkg-compatible list of reexport declarations.
    """
    exposed_reexports = []
    for dep, cabal_decls in reexported_modules.items():
        for cabal_decl in cabal_decls.split(","):
            stripped_cabal_decl = cabal_decl.strip()
            cabal_decl_parts = stripped_cabal_decl.split(" as ")
            original = cabal_decl_parts[0]
            if len(cabal_decl_parts) == 2:
                reexported = cabal_decl_parts[1]
            else:
                reexported = cabal_decl_parts[0]
            if HaskellLibraryInfo in dep:
                pkg = dep[HaskellLibraryInfo].package_id
            exposed_reexport = "{reexported} from {pkg}:{original}".format(
                reexported = reexported,
                pkg = pkg,
                original = original,
            )
            exposed_reexports.append(exposed_reexport)

    return exposed_reexports

def _get_mix_filepath(coverage_datum):
    """ Extracts mix file path from a coverage datum.
    """
    return coverage_datum.mix_file.short_path
