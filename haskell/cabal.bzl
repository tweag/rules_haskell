"""Cabal packages"""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load("//vendor/bazel_json/lib:json_parser.bzl", "json_parse")
load("@bazel_tools//tools/cpp:lib_cc_configure.bzl", "get_cpu_value")
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")
load(":cc.bzl", "cc_interop_info")
load(":private/actions/info.bzl", "library_info_output_groups")
load(":private/context.bzl", "haskell_context", "render_env")
load(":private/dependencies.bzl", "gather_dep_info")
load(":private/expansions.bzl", "expand_make_variables")
load(":private/mode.bzl", "is_profiling_enabled")
load(
    ":private/path_utils.bzl",
    "create_rpath_entry",
    "join_path_list",
    "relative_rpath_prefix",
    "truly_relativize",
)
load(":private/set.bzl", "set")
load(":private/typing.bzl", "typecheck_stackage_extradeps")
load(":haddock.bzl", "generate_unified_haddock_info")
load(
    ":private/workspace_utils.bzl",
    _execute_or_fail_loudly = "execute_or_fail_loudly",
)
load(
    ":providers.bzl",
    "HaddockInfo",
    "HaskellInfo",
    "HaskellLibraryInfo",
    "all_dependencies_package_ids",
)
load(
    ":private/cc_libraries.bzl",
    "deps_HaskellCcLibrariesInfo",
    "get_cc_libraries",
    "get_ghci_library_files",
    "get_library_files",
    "haskell_cc_libraries_aspect",
)

def _so_extension(hs):
    return "dylib" if hs.toolchain.is_darwin else "so"

def _dirname(file):
    return file.dirname

def _version(name):
    """Return the version component of a package name."""
    return name.rpartition("-")[2]

def _has_version(name):
    """Check whether a package identifier has a version component."""
    return name.rpartition("-")[2].replace(".", "").isdigit()

def _chop_version(name):
    """Remove any version component from the given package name."""
    return name.rpartition("-")[0]

def _find_cabal(hs, srcs):
    """Check that a .cabal file exists. Choose the root one."""
    cabal = None
    for f in srcs:
        if f.extension == "cabal":
            if not cabal or f.dirname < cabal.dirname:
                cabal = f
    if not cabal:
        fail("A .cabal file was not found in the srcs attribute.")
    return cabal

def _find_setup(hs, cabal, srcs):
    """Check that a Setup script exists. If not, create a default one."""
    setup = None
    for f in srcs:
        if f.basename in ["Setup.hs", "Setup.lhs"]:
            if not setup or f.dirname < setup.dirname:
                setup = f
    if not setup:
        setup = hs.actions.declare_file("Setup.hs", sibling = cabal)
        hs.actions.write(
            output = setup,
            content = """
module Main where
import Distribution.Simple

main :: IO ()
main = defaultMain
""",
        )
    return setup

_CABAL_TOOLS = ["alex", "c2hs", "cpphs", "doctest", "happy"]

# Some old packages are empty compatibility shims. Empty packages
# cause Cabal to not produce the outputs it normally produces. Instead
# of detecting that, we blacklist the offending packages, on the
# assumption that such packages are old and rare.
#
# TODO: replace this with a more general solution.
_EMPTY_PACKAGES_BLACKLIST = [
    "bytestring-builder",
    "fail",
    "mtl-compat",
    "nats",
]

def _cabal_tool_flag(tool):
    """Return a --with-PROG=PATH flag if input is a recognized Cabal tool. None otherwise."""
    if tool.basename in _CABAL_TOOLS:
        return "--with-{}={}".format(tool.basename, tool.path)

def _binary_paths(binaries):
    return [binary.dirname for binary in binaries.to_list()]

def _concat(sequences):
    return [item for sequence in sequences for item in sequence]

def _prepare_cabal_inputs(
        hs,
        cc,
        posix,
        dep_info,
        cc_info,
        direct_cc_info,
        component,
        package_id,
        tool_inputs,
        tool_input_manifests,
        cabal,
        setup,
        setup_deps,
        setup_dep_info,
        srcs,
        cabalopts,
        flags,
        generate_haddock,
        cabal_wrapper,
        package_database,
        verbose,
        transitive_haddocks,
        is_library = False,
        dynamic_file = None):
    """Compute Cabal wrapper, arguments, inputs."""
    with_profiling = is_profiling_enabled(hs)

    # Haskell library dependencies or indirect C library dependencies are
    # already covered by their corresponding package-db entries. We only need
    # to add libraries and headers for direct C library dependencies to the
    # command line.
    direct_libs = get_ghci_library_files(hs, cc.cc_libraries_info, cc.cc_libraries)

    # The regular Haskell rules perform mostly static linking, i.e. where
    # possible all C library dependencies are linked statically. Cabal has no
    # such mode, and since we have to provide dynamic C libraries for
    # compilation, they will also be used for linking. Hence, we need to add
    # RUNPATH flags for all dynamic C library dependencies. Cabal also produces
    # a dynamic and a static Haskell library in one go. The dynamic library
    # will link other Haskell libraries dynamically. For those we need to also
    # provide RUNPATH flags for dynamic Haskell libraries.
    (_, dynamic_libs) = get_library_files(
        hs,
        cc.cc_libraries_info,
        cc.transitive_libraries,
        dynamic = True,
    )

    # Executables built by Cabal will link Haskell libraries statically, so we
    # only need to include dynamic C libraries in the runfiles tree.
    (_, runfiles_libs) = get_library_files(
        hs,
        cc.cc_libraries_info,
        get_cc_libraries(cc.cc_libraries_info, cc.transitive_libraries),
        dynamic = True,
    )

    # Setup dependencies are loaded by runghc.
    setup_libs = get_ghci_library_files(hs, cc.cc_libraries_info, cc.setup_libraries)

    # The regular Haskell rules have separate actions for linking and
    # compilation to which we pass different sets of libraries as inputs. The
    # Cabal rules, in contrast, only have a single action for compilation and
    # linking, so we must provide both sets of libraries as inputs to the same
    # action.
    transitive_compile_libs = get_ghci_library_files(hs, cc.cc_libraries_info, cc.transitive_libraries)
    transitive_link_libs = _concat(get_library_files(hs, cc.cc_libraries_info, cc.transitive_libraries))
    env = dict(hs.env)
    env["PATH"] = join_path_list(hs, _binary_paths(tool_inputs) + posix.paths)
    if hs.toolchain.is_darwin:
        env["SDKROOT"] = "macosx"  # See haskell/private/actions/link.bzl

    if verbose:
        env["CABAL_VERBOSE"] = "True"

    args = hs.actions.args()
    package_databases = dep_info.package_databases
    transitive_headers = cc_info.compilation_context.headers
    direct_include_dirs = depset(transitive = [
        direct_cc_info.compilation_context.includes,
        direct_cc_info.compilation_context.quote_includes,
        direct_cc_info.compilation_context.system_includes,
    ])
    direct_lib_dirs = [file.dirname for file in direct_libs]
    args.add_all([component, package_id, generate_haddock, setup, cabal.dirname, package_database.dirname])
    args.add_joined([
        arg
        for package_id in setup_deps
        for arg in ["-package-id", package_id]
    ] + [
        arg
        for package_db in setup_dep_info.package_databases.to_list()
        for arg in ["-package-db", "./" + _dirname(package_db)]
    ], join_with = " ", format_each = "--ghc-arg=%s", omit_if_empty = False)
    args.add("--flags=" + " ".join(flags))
    if dynamic_file:
        # See Note [No PIE when linking] in haskell/private/actions/link.bzl
        if not (hs.toolchain.is_darwin or hs.toolchain.is_windows):
            version = [int(x) for x in hs.toolchain.version.split(".")]
            if version < [8, 10] or not is_library:
                args.add("--ghc-option=-optl-no-pie")
    args.add_all(hs.toolchain.cabalopts)
    args.add_all(cabalopts)
    if dynamic_file:
        args.add_all(
            [
                "--ghc-option=-optl-Wl,-rpath," + create_rpath_entry(
                    binary = dynamic_file,
                    dependency = lib,
                    keep_filename = False,
                    prefix = relative_rpath_prefix(hs.toolchain.is_darwin),
                )
                for lib in dynamic_libs
            ],
            uniquify = True,
        )

    # When building in a static context, we need to make sure that Cabal passes
    # a couple of options that ensure any static code it builds can be linked
    # correctly.
    #
    # * If we are using a static runtime, we need to ensure GHC generates
    #   position-independent code (PIC). On Unix we need to pass GHC both
    #   `-fPIC` and `-fexternal-dynamic-refs`: with `-fPIC` alone, GHC will
    #   generate `R_X86_64_PC32` relocations on Unix, which prevent loading its
    #   static libraries as PIC.
    #
    # * If we are building fully-statically-linked binaries, we need to ensure that
    #   we pass arguments to `hsc2hs` such that objects it builds are statically
    #   linked, otherwise we'll get dynamic linking errors when trying to
    #   execute those objects to generate code as part of the build.  Since the
    #   static configuration should ensure that all the objects involved are
    #   themselves statically built, this is just a case of passing `-static` to
    #   the linker used by `hsc2hs` (which will be our own wrapper script which
    #   eventually calls `gcc`, etc.).
    if hs.toolchain.static_runtime:
        args.add("--ghc-option=-fPIC")

        if not hs.toolchain.is_windows:
            args.add("--ghc-option=-fexternal-dynamic-refs")

    if hs.toolchain.fully_static_link:
        args.add("--hsc2hs-option=--lflag=-static")

    if hs.features.fully_static_link:
        args.add("--ghc-option=-optl-static")

    args.add("--")
    args.add_all(package_databases, map_each = _dirname, format_each = "--package-db=%s")
    args.add_all(direct_include_dirs, format_each = "--extra-include-dirs=%s")
    args.add_all(direct_lib_dirs, format_each = "--extra-lib-dirs=%s", uniquify = True)
    if with_profiling:
        args.add("--enable-profiling")

    # Redundant with _binary_paths() above, but better be explicit when we can.
    args.add_all(tool_inputs, map_each = _cabal_tool_flag)

    inputs = depset(
        [setup, hs.tools.ghc, hs.tools.ghc_pkg, hs.tools.runghc],
        transitive = [
            depset(srcs),
            depset(cc.files),
            package_databases,
            setup_dep_info.package_databases,
            transitive_headers,
            depset(setup_libs),
            depset(transitive_compile_libs),
            depset(transitive_link_libs),
            depset(transitive_haddocks),
            setup_dep_info.interface_dirs,
            setup_dep_info.hs_libraries,
            dep_info.interface_dirs,
            dep_info.hs_libraries,
            tool_inputs,
        ],
    )
    input_manifests = tool_input_manifests + hs.toolchain.cc_wrapper.manifests

    return struct(
        cabal_wrapper = cabal_wrapper,
        args = args,
        inputs = inputs,
        input_manifests = input_manifests,
        env = env,
        runfiles = depset(direct = runfiles_libs),
    )

def _gather_transitive_haddocks(deps):
    transitive_haddocks_list = []
    for dep in deps:
        if HaddockInfo in dep:
            for haddock_files in dep[HaddockInfo].transitive_haddocks.values():
                transitive_haddocks_list.extend(haddock_files)
    return depset(
        direct = transitive_haddocks_list,
    )

def _shorten_library_symlink(dynamic_library):
    prefix = dynamic_library.owner.workspace_root.replace("_", "_U").replace("/", "_S")
    basename = dynamic_library.basename
    return paths.join(prefix, basename)

def _haskell_cabal_library_impl(ctx):
    hs = haskell_context(ctx)
    dep_info = gather_dep_info(ctx, ctx.attr.deps)
    setup_dep_info = gather_dep_info(ctx, ctx.attr.setup_deps)
    setup_deps = all_dependencies_package_ids(ctx.attr.setup_deps)
    cc = cc_interop_info(ctx)

    # All C and Haskell library dependencies.
    cc_info = cc_common.merge_cc_infos(
        cc_infos = [dep[CcInfo] for dep in ctx.attr.deps if CcInfo in dep],
    )

    # Separate direct C library dependencies.
    direct_cc_info = cc_common.merge_cc_infos(
        cc_infos = [
            dep[CcInfo]
            for dep in ctx.attr.deps
            if CcInfo in dep and not HaskellInfo in dep
        ],
    )
    posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]
    package_name = ctx.attr.package_name if ctx.attr.package_name else hs.label.name
    package_id = "{}-{}".format(
        package_name,
        ctx.attr.version,
    )
    with_profiling = is_profiling_enabled(hs)

    user_cabalopts = _expand_make_variables("cabalopts", ctx, ctx.attr.cabalopts)
    if ctx.attr.compiler_flags:
        print("WARNING: compiler_flags attribute is deprecated. Use cabalopts instead.")
        user_cabalopts.extend([
            "--ghc-option=" + opt
            for opt in _expand_make_variables("compiler_flags", ctx, ctx.attr.compiler_flags)
        ])
    cabal = _find_cabal(hs, ctx.files.srcs)
    setup = _find_setup(hs, cabal, ctx.files.srcs)
    package_database = hs.actions.declare_file(
        "_install/{}.conf.d/package.cache".format(package_id),
        sibling = cabal,
    )
    interfaces_dir = hs.actions.declare_directory(
        "_install/{}_iface".format(package_id),
        sibling = cabal,
    )
    data_dir = hs.actions.declare_directory(
        "_install/{}_data".format(package_id),
        sibling = cabal,
    )
    if ctx.attr.haddock:
        haddock_file = hs.actions.declare_file(
            "_install/{}_haddock/{}.haddock".format(package_id, package_name),
            sibling = cabal,
        )
        haddock_html_dir = hs.actions.declare_directory(
            "_install/{}_haddock_html".format(package_id),
            sibling = cabal,
        )
    else:
        haddock_file = None
        haddock_html_dir = None
    vanilla_library = hs.actions.declare_file(
        "_install/lib/libHS{}.a".format(package_id),
        sibling = cabal,
    )
    if with_profiling:
        profiling_library = hs.actions.declare_file(
            "_install/lib/libHS{}_p.a".format(package_id),
            sibling = cabal,
        )
        static_library = profiling_library
    else:
        profiling_library = None
        static_library = vanilla_library
    if hs.toolchain.static_runtime:
        dynamic_library = None
    else:
        dynamic_library = hs.actions.declare_file(
            "_install/lib/libHS{}-ghc{}.{}".format(
                package_id,
                hs.toolchain.version,
                _so_extension(hs),
            ),
            sibling = cabal,
        )
    (tool_inputs, tool_input_manifests) = ctx.resolve_tools(tools = ctx.attr.tools)
    c = _prepare_cabal_inputs(
        hs,
        cc,
        posix,
        dep_info,
        cc_info,
        direct_cc_info,
        component = "lib:{}".format(
            ctx.attr.package_name if ctx.attr.package_name else hs.label.name,
        ),
        package_id = package_id,
        tool_inputs = tool_inputs,
        tool_input_manifests = tool_input_manifests,
        cabal = cabal,
        setup = setup,
        setup_deps = setup_deps,
        setup_dep_info = setup_dep_info,
        srcs = ctx.files.srcs,
        cabalopts = user_cabalopts,
        flags = ctx.attr.flags,
        generate_haddock = ctx.attr.haddock,
        cabal_wrapper = ctx.executable._cabal_wrapper,
        package_database = package_database,
        verbose = ctx.attr.verbose,
        is_library = True,
        dynamic_file = dynamic_library,
        transitive_haddocks = _gather_transitive_haddocks(ctx.attr.deps),
    )
    outputs = [
        package_database,
        interfaces_dir,
        vanilla_library,
        data_dir,
    ]
    if ctx.attr.haddock:
        outputs.extend([haddock_file, haddock_html_dir])
    if dynamic_library != None:
        outputs.append(dynamic_library)
    if with_profiling:
        outputs.append(profiling_library)
    ctx.actions.run(
        executable = c.cabal_wrapper,
        arguments = [c.args],
        inputs = c.inputs,
        input_manifests = c.input_manifests,
        tools = [c.cabal_wrapper],
        outputs = outputs,
        env = c.env,
        mnemonic = "HaskellCabalLibrary",
        progress_message = "HaskellCabalLibrary {}".format(hs.label),
    )

    default_info = DefaultInfo(
        files = depset([static_library] + ([dynamic_library] if dynamic_library != None else [])),
        runfiles = ctx.runfiles(
            files = [data_dir],
            collect_default = True,
        ),
    )
    hs_info = HaskellInfo(
        package_databases = depset([package_database], transitive = [dep_info.package_databases]),
        version_macros = set.empty(),
        source_files = depset(),
        extra_source_files = depset(),
        import_dirs = set.empty(),
        hs_libraries = depset(
            direct = [lib for lib in [vanilla_library, dynamic_library, profiling_library] if lib],
            transitive = [dep_info.hs_libraries],
            order = "topological",
        ),
        interface_dirs = depset([interfaces_dir], transitive = [dep_info.interface_dirs]),
        compile_flags = [],
        user_compile_flags = [],
        user_repl_flags = [],
    )
    lib_info = HaskellLibraryInfo(package_id = package_id, version = None, exports = [])
    if ctx.attr.haddock:
        doc_info = generate_unified_haddock_info(
            this_package_id = package_id,
            this_package_html = haddock_html_dir,
            this_package_haddock = haddock_file,
            deps = ctx.attr.deps,
        )
    else:
        doc_info = None
    cc_toolchain = find_cpp_toolchain(ctx)
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )
    linker_input = cc_common.create_linker_input(
        owner = ctx.label,
        libraries = depset(direct = [
            cc_common.create_library_to_link(
                actions = ctx.actions,
                feature_configuration = feature_configuration,
                dynamic_library = dynamic_library,
                dynamic_library_symlink_path =
                    _shorten_library_symlink(dynamic_library) if dynamic_library and ctx.attr.unique_name else "",
                static_library = static_library,
                cc_toolchain = cc_toolchain,
            ),
        ]),
    )
    compilation_context = cc_common.create_compilation_context()
    linking_context = cc_common.create_linking_context(
        linker_inputs = depset(direct = [linker_input]),
    )
    cc_info = cc_common.merge_cc_infos(
        cc_infos = [
            CcInfo(
                compilation_context = compilation_context,
                linking_context = linking_context,
            ),
            cc_info,
        ],
    )
    output_group_info = OutputGroupInfo(**library_info_output_groups(
        name = ctx.label.name,
        hs = hs,
        hs_info = hs_info,
        lib_info = lib_info,
    ))
    result = [default_info, hs_info, cc_info, lib_info, output_group_info]
    if ctx.attr.haddock:
        result.append(doc_info)
    return result

haskell_cabal_library = rule(
    _haskell_cabal_library_impl,
    attrs = {
        "package_name": attr.string(
            doc = "Cabal package name. Defaults to name attribute.",
        ),
        "version": attr.string(
            doc = "Version of the Cabal package.",
            mandatory = True,
        ),
        "haddock": attr.bool(
            default = True,
            doc = "Whether to generate haddock documentation.",
        ),
        "srcs": attr.label_list(
            allow_files = True,
            doc = "All files required to build the package, including the Cabal file.",
        ),
        "deps": attr.label_list(
            aspects = [haskell_cc_libraries_aspect],
            doc = "Package build dependencies. Note, setup dependencies need to be declared separately using `setup_deps`.",
        ),
        "setup_deps": attr.label_list(
            aspects = [haskell_cc_libraries_aspect],
            doc = "Dependencies for custom setup Setup.hs.",
        ),
        "cabalopts": attr.string_list(
            doc = """Additional flags to pass to `Setup.hs configure`. Subject to make variable expansion.

            Use `--ghc-option=OPT` to configure additional compiler flags.
            Use `--haddock-option=--optghc=OPT` if these flags are required for haddock generation as well.
            """,
        ),
        "compiler_flags": attr.string_list(
            doc = """DEPRECATED. Use `cabalopts` with `--ghc-option` instead.

            Flags to pass to Haskell compiler, in addition to those defined the cabal file. Subject to Make variable substitution.""",
        ),
        "tools": attr.label_list(
            cfg = "host",
            allow_files = True,
            doc = """Tool dependencies. They are built using the host configuration, since
            the tools are executed as part of the build.""",
        ),
        "flags": attr.string_list(
            doc = "List of Cabal flags, will be passed to `Setup.hs configure --flags=...`.",
        ),
        "_cabal_wrapper": attr.label(
            executable = True,
            cfg = "host",
            default = Label("@rules_haskell//haskell:cabal_wrapper"),
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
        "verbose": attr.bool(
            default = True,
            doc = "Whether to show the output of the build",
        ),
        "unique_name": attr.bool(
            default = False,
            doc = """Whether the library name is known to be unique within the
            workspace. This is used by `stack_snapshot` where library names are
            known to be unique within the snapshot. If true, then the dynamic
            library symlink underneath `_solib_<cpu>` will be shortened to
            avoid exceeding the MACH-O header size limit on MacOS.""",
        ),
    },
    toolchains = [
        "@bazel_tools//tools/cpp:toolchain_type",
        "@rules_haskell//haskell:toolchain",
        "@rules_sh//sh/posix:toolchain_type",
    ],
    fragments = ["cpp"],
    doc = """\
Use Cabal to build a library.

### Examples

  ```bzl
  haskell_cabal_library(
      name = "lib-0.1.0.0",
      srcs = ["lib.cabal", "Lib.hs", "Setup.hs"],
  )

  haskell_toolchain_library(name = "base")

  haskell_binary(
      name = "bin",
      deps = [":base", ":lib-0.1.0.0"],
      srcs = ["Main.hs"],
  )
  ```

This rule does not use `cabal-install`. It calls the package's
`Setup.hs` script directly if one exists, or the default one if not.
All sources files that would have been part of a Cabal sdist need to
be listed in `srcs` (crucially, including the `.cabal` file).
A `haskell_cabal_library` can be substituted for any
`haskell_library`. The two are interchangeable in most contexts.
However, using a plain `haskell_library` sometimes leads to better
build times, and does not require drafting a `.cabal` file.

""",
)

def _haskell_cabal_binary_impl(ctx):
    hs = haskell_context(ctx)
    dep_info = gather_dep_info(ctx, ctx.attr.deps)
    setup_dep_info = gather_dep_info(ctx, ctx.attr.setup_deps)
    setup_deps = all_dependencies_package_ids(ctx.attr.setup_deps)
    cc = cc_interop_info(ctx)

    # All C and Haskell library dependencies.
    cc_info = cc_common.merge_cc_infos(
        cc_infos = [dep[CcInfo] for dep in ctx.attr.deps if CcInfo in dep],
    )

    # Separate direct C library dependencies.
    direct_cc_info = cc_common.merge_cc_infos(
        cc_infos = [
            dep[CcInfo]
            for dep in ctx.attr.deps
            if CcInfo in dep and not HaskellInfo in dep
        ],
    )
    posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]

    exe_name = ctx.attr.exe_name if ctx.attr.exe_name else hs.label.name
    user_cabalopts = _expand_make_variables("cabalopts", ctx, ctx.attr.cabalopts)
    if ctx.attr.compiler_flags:
        print("WARNING: compiler_flags attribute is deprecated. Use cabalopts instead.")
        user_cabalopts.extend([
            "--ghc-option=" + opt
            for opt in _expand_make_variables("compiler_flags", ctx, ctx.attr.compiler_flags)
        ])
    cabal = _find_cabal(hs, ctx.files.srcs)
    setup = _find_setup(hs, cabal, ctx.files.srcs)
    package_database = hs.actions.declare_file(
        "_install/{}.conf.d/package.cache".format(hs.label.name),
        sibling = cabal,
    )
    binary = hs.actions.declare_file(
        "_install/bin/{name}{ext}".format(
            name = exe_name,
            ext = ".exe" if hs.toolchain.is_windows else "",
        ),
        sibling = cabal,
    )
    data_dir = hs.actions.declare_directory(
        "_install/{}_data".format(hs.label.name),
        sibling = cabal,
    )
    (tool_inputs, tool_input_manifests) = ctx.resolve_tools(tools = ctx.attr.tools)
    c = _prepare_cabal_inputs(
        hs,
        cc,
        posix,
        dep_info,
        cc_info,
        direct_cc_info,
        component = "exe:{}".format(exe_name),
        package_id = hs.label.name,
        tool_inputs = tool_inputs,
        tool_input_manifests = tool_input_manifests,
        cabal = cabal,
        setup = setup,
        setup_deps = setup_deps,
        setup_dep_info = setup_dep_info,
        srcs = ctx.files.srcs,
        cabalopts = user_cabalopts,
        flags = ctx.attr.flags,
        generate_haddock = False,
        cabal_wrapper = ctx.executable._cabal_wrapper,
        package_database = package_database,
        verbose = ctx.attr.verbose,
        dynamic_file = binary,
        transitive_haddocks = _gather_transitive_haddocks(ctx.attr.deps),
    )
    ctx.actions.run(
        executable = c.cabal_wrapper,
        arguments = [c.args],
        inputs = c.inputs,
        input_manifests = c.input_manifests,
        outputs = [
            package_database,
            binary,
            data_dir,
        ],
        tools = [c.cabal_wrapper],
        env = c.env,
        mnemonic = "HaskellCabalBinary",
        progress_message = "HaskellCabalBinary {}".format(hs.label),
    )

    hs_info = HaskellInfo(
        package_databases = dep_info.package_databases,
        version_macros = set.empty(),
        source_files = depset(),
        extra_source_files = depset(),
        import_dirs = set.empty(),
        hs_libraries = dep_info.hs_libraries,
        interface_dirs = dep_info.interface_dirs,
        compile_flags = [],
        user_compile_flags = [],
        user_repl_flags = [],
    )
    default_info = DefaultInfo(
        files = depset([binary]),
        executable = binary,
        runfiles = ctx.runfiles(
            files = [data_dir],
            transitive_files = c.runfiles,
            collect_default = True,
        ),
    )

    return [hs_info, cc_info, default_info]

haskell_cabal_binary = rule(
    _haskell_cabal_binary_impl,
    executable = True,
    attrs = {
        "exe_name": attr.string(
            doc = "Cabal executable component name. Defaults to the value of the name attribute.",
        ),
        "srcs": attr.label_list(
            allow_files = True,
            doc = "All files required to build the package, including the Cabal file.",
        ),
        "deps": attr.label_list(
            aspects = [haskell_cc_libraries_aspect],
            doc = "Package build dependencies. Note, setup dependencies need to be declared separately using `setup_deps`.",
        ),
        "setup_deps": attr.label_list(
            aspects = [haskell_cc_libraries_aspect],
            doc = "Dependencies for custom setup Setup.hs.",
        ),
        "cabalopts": attr.string_list(
            doc = """Additional flags to pass to `Setup.hs configure`. Subject to make variable expansion.

            Use `--ghc-option=OPT` to configure additional compiler flags.
            Use `--haddock-option=--optghc=OPT` if these flags are required for haddock generation as well.
            """,
        ),
        "compiler_flags": attr.string_list(
            doc = """DEPRECATED. Use `cabalopts` with `--ghc-option` instead.

            Flags to pass to Haskell compiler, in addition to those defined the cabal file. Subject to Make variable substitution.""",
        ),
        "tools": attr.label_list(
            cfg = "host",
            allow_files = True,
            doc = """Tool dependencies. They are built using the host configuration, since
            the tools are executed as part of the build.""",
        ),
        "flags": attr.string_list(
            doc = "List of Cabal flags, will be passed to `Setup.hs configure --flags=...`.",
        ),
        "_cabal_wrapper": attr.label(
            executable = True,
            cfg = "host",
            default = Label("@rules_haskell//haskell:cabal_wrapper"),
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
        "verbose": attr.bool(
            default = True,
            doc = "Whether to show the output of the build",
        ),
    },
    toolchains = [
        "@bazel_tools//tools/cpp:toolchain_type",
        "@rules_haskell//haskell:toolchain",
        "@rules_sh//sh/posix:toolchain_type",
    ],
    fragments = ["cpp"],
    doc = """\
Use Cabal to build a binary.

### Examples

  ```bzl
  haskell_cabal_binary(
      name = "happy",
      srcs = glob(["**"]),
  )
  ```

This rule assumes that the .cabal file defines a single executable
with the same name as the package.

This rule does not use `cabal-install`. It calls the package's
`Setup.hs` script directly if one exists, or the default one if not.
All sources files that would have been part of a Cabal sdist need to
be listed in `srcs` (crucially, including the `.cabal` file).

""",
)

# Temporary hardcoded list of core libraries. This will no longer be
# necessary once Stack 2.0 is released.
#
# TODO remove this list and replace it with Stack's --global-hints
# mechanism.
_CORE_PACKAGES = [
    "Cabal",
    "array",
    "base",
    "binary",
    "bytestring",
    "containers",
    "deepseq",
    "directory",
    "filepath",
    "ghc",
    "ghc-boot",
    "ghc-boot-th",
    "ghc-compact",
    "ghc-heap",
    "ghc-prim",
    "ghci",
    "haskeline",
    "hpc",
    "integer-gmp",
    "integer-simple",
    "libiserv",
    "mtl",
    "parsec",
    "pretty",
    "process",
    "rts",
    "stm",
    "template-haskell",
    "terminfo",
    "text",
    "time",
    "transformers",
    "unix",
    "Win32",
    "xhtml",
]

_STACK_DEFAULT_VERSION = "2.3.1"

# Only ever need one version, but use same structure as for GHC bindists.
_STACK_BINDISTS = \
    {
        "2.3.1": {
            "linux-x86_64": (
                "https://github.com/commercialhaskell/stack/releases/download/v2.3.1/stack-2.3.1-linux-x86_64-static.tar.gz",
                "4bae8830b2614dddf3638a6d1a7bbbc3a5a833d05b2128eae37467841ac30e47",
            ),
            "osx-x86_64": (
                "https://github.com/commercialhaskell/stack/releases/download/v2.3.1/stack-2.3.1-osx-x86_64.tar.gz",
                "73eee7e5f24d11fd0af00cb05f16119e86be5d578c35083250e6b85ed1ca3621",
            ),
            "windows-x86_64": (
                "https://github.com/commercialhaskell/stack/releases/download/v2.3.1/stack-2.3.1-windows-x86_64.tar.gz",
                "440588c92ffcb42b88fd6455dc68728dae9b08bdd1a683d1cf5f80aa9aa8b014",
            ),
        },
    }

def _stack_version_check(repository_ctx, stack_cmd):
    """Returns False if version not recent enough."""
    exec_result = _execute_or_fail_loudly(repository_ctx, [stack_cmd, "--numeric-version"])

    stack_major_version = int(exec_result.stdout.split(".")[0])
    stack_minor_version = int(exec_result.stdout.split(".")[1])
    return stack_major_version >= 2 and stack_minor_version >= 3

def _parse_components(package, components):
    """Parse and validate a list of Cabal components.

    Components take the following shape:
      * `lib`: The library component.
      * `lib:<package>`: The library component.
      * `exe`: The executable component `exe:<package>`.
      * `exe:<name>`: An executable component.

    Args:
      package: string, The package name.
      components: list of string, The Cabal components

    Returns:
      struct(lib, exe):
        lib: bool, Whether the package has a library component.
        exe: list of string, List of executables.
    """
    lib = False
    exe = []

    for component in components:
        if component == "lib":
            lib = True
        elif component.startswith("lib:"):
            if component == "lib:%s" % package:
                lib = True
            else:
                fail("Sublibrary components are not supported: %s in %s" % (component, package), "components")
        elif component == "exe":
            exe.append(package)
        elif component.startswith("exe:"):
            exe.append(component[4:])
        elif component.startswith("test"):
            fail("Cabal test components are not supported: %s in %s" % (component, package), "components")
        else:
            fail("Invalid Cabal component: %s in %s" % (component, package), "components")

    if package in _CORE_PACKAGES:
        if not lib or exe != []:
            fail("Invalid core package components: %s" % package, "components")

    return struct(lib = lib, exe = exe)

_default_components = {
    "alex": struct(lib = False, exe = ["alex"]),
    "c2hs": struct(lib = False, exe = ["c2hs"]),
    "cpphs": struct(lib = True, exe = ["cpphs"]),
    "doctest": struct(lib = True, exe = ["doctest"]),
    "happy": struct(lib = False, exe = ["happy"]),
}

def _get_components(components, package):
    """Look-up the components of a package.

    If the package is not listed in the user-defined components then it
    will be taken from the `_default_components`. If it is not listed
    there then it will default to a library and no executable components.
    """
    return components.get(package, _default_components.get(package, struct(lib = True, exe = [])))

def _parse_json_field(json, field, ty, errmsg):
    """Read and type-check a field from a JSON object.

    Invokes `fail` on error.

    Attrs:
      json: dict, The parsed JSON object.
      field: string, The name of the field.
      ty: string, The expected type of the field.
      errmsg: string, Error message format string. E.g. `Error: {error}`.

    Returns:
      The value of the field.
    """
    if not field in json:
        fail(errmsg.format(error = "Missing field '{field}'.".format(field = field)))
    actual_ty = type(json[field])
    if actual_ty != ty:
        fail(errmsg.format(error = "Expected field '{field}' of type '{expected}', but got '{got}'.".format(
            field = field,
            expected = ty,
            got = actual_ty,
        )))
    return json[field]

def _parse_package_spec(package_spec):
    """Parse a package description from `stack ls dependencies json`.

    The definition of the JSON format can be found in the `stack` sources:
    https://github.com/commercialhaskell/stack/blob/v2.3.1/src/Stack/Dot.hs#L173-L198
    """
    errmsg = "Unexpected output format for `stack ls dependencies json` in {context}: {{error}}"

    # Parse simple fields.
    parsed = {
        field: _parse_json_field(
            package_spec,
            field,
            ty,
            errmsg.format(context = "package description"),
        )
        for (field, ty) in [("name", "string"), ("version", "string"), ("dependencies", "list")]
    }

    # Parse location field.
    location = {}
    if parsed["name"] in _CORE_PACKAGES or not "location" in package_spec:
        location["type"] = "core"
    else:
        location_type = _parse_json_field(
            json = package_spec["location"],
            field = "type",
            ty = "string",
            errmsg = errmsg.format(context = "location description"),
        )
        if location_type == "project package":
            location["type"] = "vendored"
        elif location_type == "hackage":
            location["type"] = location_type
            url_prefix = _parse_json_field(
                json = package_spec["location"],
                field = "url",
                ty = "string",
                errmsg = errmsg.format(context = location_type + " location description"),
            )
            location["url"] = url_prefix + "/{name}-{version}.tar.gz".format(**parsed)
            # stack does not expose sha-256, see https://github.com/commercialhaskell/stack/issues/5274

        elif location_type == "archive":
            location["type"] = location_type
            location["url"] = _parse_json_field(
                json = package_spec["location"],
                field = "url",
                ty = "string",
                errmsg = errmsg.format(context = location_type + " location description"),
            )
            # stack does not yet expose sha-256, see https://github.com/commercialhaskell/stack/pull/5280

        elif location_type in ["git", "hg"]:
            location["type"] = location_type
            location["url"] = _parse_json_field(
                json = package_spec["location"],
                field = "url",
                ty = "string",
                errmsg = errmsg.format(context = location_type + " location description"),
            )
            location["commit"] = _parse_json_field(
                json = package_spec["location"],
                field = "commit",
                ty = "string",
                errmsg = errmsg.format(context = location_type + " location description"),
            )
            location["subdir"] = _parse_json_field(
                json = package_spec["location"],
                field = "subdir",
                ty = "string",
                errmsg = errmsg.format(context = location_type + " location description"),
            )
        else:
            error = "Unexpected location type '{}'.".format(location_type)
            fail(errmsg.format(context = "location description").format(error = error))

    parsed["location"] = location

    return parsed

def _resolve_packages(
        repository_ctx,
        snapshot,
        core_packages,
        versioned_packages,
        unversioned_packages,
        vendored_packages):
    """Invoke stack to determine package versions and dependencies.

    Attrs:
      snapshot: The Stackage snapshot, name or path to custom snapshot.
      core_packages: Core packages requested by the user.
      versioned_packages: Versioned packages requested by the user.
      unversioned_packages: Unversioned packages requested by the user.
      vendored_packages: Vendored packages provided by the user.

    Returns:
      dict(name: dict(name, version, dependencies, location)):
        name: string, The unversioned package name.
        version: string, The version of the package.
        dependencies: list, Package dependencies.
        location: dict(type, url?, sha256?, commit?, subdir?):
          type: One of "core", "vendored", "hackage", "archive", "git", "hg".
    """

    # Create a dummy package depending on all requested packages.
    resolve_package = "rules-haskell-stack-resolve"
    repository_ctx.file(
        "{name}/{name}.cabal".format(name = resolve_package),
        executable = False,
        content = """\
name: {name}
cabal-version: >= 1.2
version: 1.0
library
  build-depends:
    {packages}
""".format(
            name = resolve_package,
            packages = ",\n    ".join(core_packages + unversioned_packages + vendored_packages.keys() + [
                _chop_version(pkg)
                for pkg in versioned_packages
            ]),
        ),
    )

    # Create a stack.yaml capturing user overrides to the snapshot.
    stack_yaml_content = struct(**{
        "resolver": str(snapshot),
        "packages": [resolve_package] + [
            # Determines path to vendored package's root directory relative to
            # stack.yaml. Note, this requires that the Cabal file exists in the
            # package root and is called `<name>.cabal`.
            truly_relativize(
                str(repository_ctx.path(label.relative(name + ".cabal")).dirname),
                relative_to = str(repository_ctx.path("stack.yaml").dirname),
            )
            for (name, label) in vendored_packages.items()
        ],
        "extra-deps": versioned_packages,
        "flags": {
            pkg: {
                flag[1:] if flag.startswith("-") else flag: not flag.startswith("-")
                for flag in flags
            }
            for (pkg, flags) in repository_ctx.attr.flags.items()
        },
    }).to_json()
    repository_ctx.file("stack.yaml", content = stack_yaml_content, executable = False)

    # Invoke stack to calculate the transitive dependencies.
    stack_cmd = repository_ctx.path(repository_ctx.attr.stack)
    if not _stack_version_check(repository_ctx, stack_cmd):
        fail("Stack version not recent enough. Need version 2.3 or newer.")
    stack = [stack_cmd]
    exec_result = _execute_or_fail_loudly(
        repository_ctx,
        stack + ["ls", "dependencies", "json", "--global-hints", "--external"],
    )
    package_specs = json_parse(exec_result.stdout)

    resolved = {}
    for package_spec in package_specs:
        parsed_spec = _parse_package_spec(package_spec)
        if parsed_spec["name"] == resolve_package:
            continue
        resolved[parsed_spec["name"]] = parsed_spec

    # Sort the items to make sure that generated outputs are deterministic.
    return {name: resolved[name] for name in sorted(resolved.keys())}

def _pin_packages(repository_ctx, resolved):
    """Pin resolved packages.

    Extends the package specs with reproducibility information. In case of
    archive dependencies the SHA-256 of the fetched archive. In case of Hackage
    dependencies, the SHA-256 of the archive as well as the SHA-256 of the
    Cabal file of the latest Hackage revision.

    Cabal file downloads are not reproducible due to Hackage revisions. Here we
    pin the git revision of all-cabal-hashes that contains the used Cabal files.

    Returns:
      (all-cabal-hashes, resolved)
        all-cabal-hashes: URL to the current revision of all-cabal-hashes.
        resolved: the `resolved` argument, extended with reproducibility information.
    """
    errmsg = "Unexpected format in {context}: {{error}}"

    # Determine current git revision of all-cabal-hashes.
    repository_ctx.download(
        "https://api.github.com/repos/commercialhaskell/all-cabal-hashes/git/ref/heads/hackage",
        output = "all-cabal-hashes-hackage.json",
        executable = False,
    )
    hashes_json = json_parse(repository_ctx.read("all-cabal-hashes-hackage.json"))
    hashes_object = _parse_json_field(
        json = hashes_json,
        field = "object",
        ty = "dict",
        errmsg = errmsg.format(context = "all-cabal-hashes hackage branch"),
    )
    hashes_commit = _parse_json_field(
        json = hashes_object,
        field = "sha",
        ty = "string",
        errmsg = errmsg.format(context = "all-cabal-hashes hackage branch"),
    )
    hashes_url = "https://raw.githubusercontent.com/commercialhaskell/all-cabal-hashes/" + hashes_commit

    resolved = dict(**resolved)
    for (name, spec) in resolved.items():
        # Determine package sha256
        if spec["location"]["type"] == "hackage":
            # stack does not expose sha256, see https://github.com/commercialhaskell/stack/issues/5274
            # instead we fetch the sha256 from the all-cabal-hashes repository.
            json_url = "{url}/{name}/{version}/{name}.json".format(url = hashes_url, **spec)
            repository_ctx.download(
                json_url,
                output = "{name}-{version}.json".format(**spec),
                executable = False,
            )
            json = json_parse(repository_ctx.read("{name}-{version}.json".format(**spec)))
            hashes = _parse_json_field(
                json = json,
                field = "package-hashes",
                ty = "dict",
                errmsg = errmsg.format(context = "all-cabal-hashes package description"),
            )

            # Cabal file downloads are not reproducible due to Hackage revisions.
            # We pin the git revision of all-cabal-hashes and sha256 of the Cabal file.
            cabal_url = "{url}/{name}/{version}/{name}.cabal".format(url = hashes_url, **spec)
            spec["pinned"] = {
                "url": _parse_json_field(
                    json = json,
                    field = "package-locations",
                    ty = "list",
                    errmsg = errmsg.format(context = "all-cabal-hashes package description"),
                ),
                "sha256": _parse_json_field(
                    hashes,
                    "SHA256",
                    "string",
                    errmsg = errmsg.format(context = "all-cabal-hashes package hashes"),
                ),
                "cabal-sha256": repository_ctx.download(
                    cabal_url,
                    output = "{name}-{version}.cabal".format(**spec),
                    executable = False,
                ).sha256,
            }
        elif spec["location"]["type"] == "archive":
            # stack does not yet expose sha-256, see https://github.com/commercialhaskell/stack/pull/5280
            # instead we fetch the archive and let Bazel calculate the sha256.
            sha256 = repository_ctx.download_and_extract(
                spec["location"]["url"],
                output = "{name}-{version}".format(**spec),
            ).sha256

            # stack also doesn't expose any subdirectories that need to be
            # stripped. Here we assume the project root to be the directory
            # that contains the cabal file.
            root = "{name}-{version}".format(**spec)
            cabal_file = "{name}.cabal".format(**spec)
            find_cmd = ["find", root, "-name", cabal_file]
            if get_cpu_value(repository_ctx) == "x64_windows":
                find_cmd = ["dir", "/s", "/b", root + "\\" + cabal_file]
            subdirs = [
                paths.relativize(line.strip(), root)
                for line in _execute_or_fail_loudly(repository_ctx, [
                    "find",
                    root,
                    "-name",
                    cabal_file,
                ]).stdout.splitlines()
                if line.strip() != ""
            ]
            if len(subdirs) != 1:
                fail("Unsupported archive format at {url}: Could not find {cabal} in the archive.".format(
                    url = spec["location"]["url"],
                    cabal = cabal_file,
                ))

            spec["pinned"] = {
                "sha256": sha256,
                "strip-prefix": subdirs[0],
            }
        elif spec["location"]["type"] in ["git", "hg"]:
            # Bazel cannot cache git (or hg) repositories in the repository
            # cache as of now. Therefore, we fall back to fetching them using
            # stack rather than Bazel.
            # See https://github.com/bazelbuild/bazel/issues/5086
            pass

    return (hashes_url, resolved)

def _download_packages(repository_ctx, snapshot, pinned):
    """Downlad all remote packages.

    Downloads hackage and archive packages using Bazel, eligible to repository
    cache. Downloads git and hg packages using `stack unpack`, not eligible to
    repository cache.
    """
    stack_unpack = {}
    hashes_url = pinned["all-cabal-hashes"]

    # Unpack hackage and archive packages.
    for package in pinned["resolved"].values():
        if package["location"]["type"] == "hackage":
            repository_ctx.download_and_extract(
                package["pinned"]["url"],
                output = "{name}-{version}".format(**package),
                sha256 = package["pinned"]["sha256"],
                stripPrefix = "{name}-{version}".format(**package),
            )

            # Overwrite the Cabal file with the pinned revision.
            repository_ctx.download(
                "{url}/{name}/{version}/{name}.cabal".format(url = hashes_url, **package),
                output = "{name}-{version}/{name}.cabal".format(**package),
                sha256 = package["pinned"]["cabal-sha256"],
                executable = False,
            )
        elif package["location"]["type"] == "archive":
            repository_ctx.download_and_extract(
                package["location"]["url"],
                output = "{name}-{version}".format(**package),
                sha256 = package["pinned"]["sha256"],
                stripPrefix = package["pinned"]["strip-prefix"],
            )
        elif package["location"]["type"] in ["git", "hg"]:
            # Unpack remote packages.
            #
            # Bazel cannot cache git (or hg) repositories in the repository
            # cache as of now. Therefore, we fall back to fetching them using
            # stack rather than Bazel.
            # See https://github.com/bazelbuild/bazel/issues/5086
            #
            # TODO: Implement this using Bazel to avoid `stack update`.
            stack_unpack[package["name"]] = package

    if stack_unpack:
        # Enforce dependency on stack_update.
        # Hard-coded label since `stack_update` is `None` in this case.
        repository_ctx.read(Label("@rules_haskell_stack_update//:stack_update"))
        _download_packages_unpinned(repository_ctx, snapshot, stack_unpack)

def _download_packages_unpinned(repository_ctx, snapshot, resolved):
    """Download remote packages using `stack unpack`."""
    remote_packages = [
        package["name"]
        for package in resolved.values()
        if package["location"]["type"] not in ["core", "vendored"]
    ]
    stack = [repository_ctx.path(repository_ctx.attr.stack)]
    if remote_packages:
        _execute_or_fail_loudly(repository_ctx, stack + ["--resolver", snapshot, "unpack"] + remote_packages)

def _invert(d):
    """Invert a dictionary."""
    return dict(zip(d.values(), d.keys()))

def _from_string_keyed_label_list_dict(d):
    """Convert string_keyed_label_list_dict to label_keyed_string_dict."""

    # TODO Remove _from_string_keyed_label_list_dict once following issue
    # is resolved: https://github.com/bazelbuild/bazel/issues/7989.
    out = {}
    for (string_key, label_list) in d.items():
        for label in label_list:
            if label in out:
                out[label] += " " + string_key
            else:
                out[label] = string_key
    return out

def _to_string_keyed_label_list_dict(d):
    """Convert label_keyed_string_dict to string_keyed_label_list_dict."""

    # TODO Remove _to_string_keyed_label_list_dict once following issue
    # is resolved: https://github.com/bazelbuild/bazel/issues/7989.
    out = {}
    for (label, string_key_list) in d.items():
        for string_key in string_key_list.split(" "):
            out.setdefault(string_key, []).append(label)
    return out

def _label_to_string(label):
    return "@{}//{}:{}".format(label.workspace_name, label.package, label.name)

def _parse_stack_snapshot(repository_ctx, snapshot, local_snapshot):
    if snapshot and local_snapshot:
        fail("Please specify either snapshot or local_snapshot, but not both.")
    elif snapshot:
        snapshot = snapshot
    elif local_snapshot:
        snapshot = repository_ctx.path(local_snapshot)
    else:
        fail("Please specify one of snapshot or local_snapshot")
    return snapshot

def _parse_packages_list(packages, vendored_packages):
    """Parse the `packages` attribute to `stack_snapshot`.

    Validates that there are no duplicates between `packages` and
    `vendored_packages`.

    Args:
      packages: The list of requested packages versioned or unversioned.
      vendored_packages: The dict of provided vendored packages.

    Returns:
      struct(all, core, versioned, unversioned):
        all: The unversioned names of all requested packages.
        core: The unversioned names of requested core packages.
        versioned: The versioned names of requested versioned packages.
        unversioned: The unversioned names of requested unversioned packages.
    """
    all_packages = []
    core_packages = []
    versioned_packages = []
    unversioned_packages = []

    for package in packages:
        has_version = _has_version(package)
        unversioned = _chop_version(package) if has_version else package
        if unversioned in vendored_packages:
            fail("Duplicate package '{}'. Packages may not be listed in both 'packages' and 'vendored_packages'.".format(package))
        all_packages.append(unversioned)
        if unversioned in _CORE_PACKAGES:
            if has_version:
                fail("{} is a core package, built into GHC. Its version is determined entirely by the version of GHC you are using. You cannot pin it to {}.".format(unversioned, _version(package)))
            core_packages.append(unversioned)
        elif has_version:
            versioned_packages.append(package)
        else:
            unversioned_packages.append(package)

    return struct(
        all = all_packages,
        core = core_packages,
        versioned = versioned_packages,
        unversioned = unversioned_packages,
    )

def _pretty_print_kvs(level, kvs):
    """Write a dict in a human and diff friendly format."""
    return "{{\n{lines}\n{indent}}}".format(
        indent = level * 2 * " ",
        lines = ",\n".join([
            '{indent}"{key}": {value}'.format(
                indent = (level + 1) * 2 * " ",
                key = k,
                value = v,
            )
            for (k, v) in kvs.items()
        ]),
    )

def _snapshot_json_checksum(all_cabal_hashes, resolved):
    return hash(repr({
        "all-cabal-hashes": all_cabal_hashes,
        "resolved": resolved,
    }))

def _write_snapshot_json(repository_ctx, all_cabal_hashes, resolved):
    """Write a snapshot.json file into the remote repository root.

    Includes a checksum of the stored data used for validation on loading.
    """
    checksum = _snapshot_json_checksum(all_cabal_hashes, resolved)
    repository_ctx.file(
        "snapshot.json",
        executable = False,
        # Write one package per line sorted by name to be reproducible and diff
        # friendly. The order is ensured when constructing the dictionary and
        # preserved by the `items` iterator.
        content = _pretty_print_kvs(0, {
            "__GENERATED_FILE_DO_NOT_MODIFY_MANUALLY": checksum,
            "all-cabal-hashes": repr(all_cabal_hashes),
            "resolved": _pretty_print_kvs(1, {
                name: struct(**spec).to_json()
                for (name, spec) in resolved.items()
            }),
        }),
    )

def _read_snapshot_json(repository_ctx, filename):
    """Load a snapshot.json file.

    Validates the required fields and checks the stored checksum to detect file
    corruption or manual modification.
    """
    errmsg = """\
Failed to read {filename}: {{error}}

The file is generated and should not be modified manually, it may be corrupted.
Try to regenerate it by running the following command:

  bazel run @{workspace}-unpinned//:pin

""".format(filename = filename, workspace = repository_ctx.name)

    # Parse JSON
    pinned = json_parse(
        repository_ctx.read(repository_ctx.attr.stack_snapshot_json),
        fail_on_invalid = False,
    )
    if pinned == None:
        fail(errmsg.format(error = "Failed to parse JSON."))

    # Read snapshot.json data and validate required fields.
    expected_checksum = _parse_json_field(
        json = pinned,
        field = "__GENERATED_FILE_DO_NOT_MODIFY_MANUALLY",
        ty = "int",
        errmsg = errmsg,
    )
    all_cabal_hashes = _parse_json_field(
        json = pinned,
        field = "all-cabal-hashes",
        ty = "string",
        errmsg = errmsg,
    )
    raw_resolved = _parse_json_field(
        json = pinned,
        field = "resolved",
        ty = "dict",
        errmsg = errmsg,
    )
    resolved = {}
    for name in sorted(raw_resolved.keys()):
        raw_spec = raw_resolved[name]
        spec = {
            field: _parse_json_field(raw_spec, field, ty, errmsg)
            for (field, ty) in [("name", "string"), ("version", "string"), ("dependencies", "list")]
        }

        raw_location = _parse_json_field(
            json = raw_spec,
            field = "location",
            ty = "dict",
            errmsg = errmsg,
        )
        location_type = _parse_json_field(
            json = raw_location,
            field = "type",
            ty = "string",
            errmsg = errmsg,
        )
        if location_type in ["core", "vendored"]:
            spec["location"] = {"type": location_type}
        elif location_type in ["hackage", "archive"]:
            spec["location"] = {
                "type": location_type,
                "url": _parse_json_field(
                    json = raw_location,
                    field = "url",
                    ty = "string",
                    errmsg = errmsg,
                ),
            }
        elif location_type in ["git", "hg"]:
            spec["location"] = {
                "type": location_type,
                "url": _parse_json_field(
                    json = raw_location,
                    field = "url",
                    ty = "string",
                    errmsg = errmsg,
                ),
                "commit": _parse_json_field(
                    json = raw_location,
                    field = "commit",
                    ty = "string",
                    errmsg = errmsg,
                ),
                "subdir": _parse_json_field(
                    json = raw_location,
                    field = "subdir",
                    ty = "string",
                    errmsg = errmsg,
                ),
            }
        else:
            fail(errmsg.format(error = "Unknown location type '{}'.".format(location_type)))

        if location_type == "hackage":
            raw_pinned = _parse_json_field(
                json = raw_spec,
                field = "pinned",
                ty = "dict",
                errmsg = errmsg,
            )
            spec["pinned"] = {
                field: _parse_json_field(raw_pinned, field, ty, errmsg)
                for (field, ty) in [("url", "list"), ("sha256", "string"), ("cabal-sha256", "string")]
            }
        elif location_type == "archive":
            raw_pinned = _parse_json_field(
                json = raw_spec,
                field = "pinned",
                ty = "dict",
                errmsg = errmsg,
            )
            spec["pinned"] = {
                "sha256": _parse_json_field(
                    json = raw_pinned,
                    field = "sha256",
                    ty = "string",
                    errmsg = errmsg,
                ),
                "strip-prefix": _parse_json_field(
                    json = raw_pinned,
                    field = "strip-prefix",
                    ty = "string",
                    errmsg = errmsg,
                ),
            }

        resolved[name] = spec

    # Check the stored checksum.
    actual_checksum = _snapshot_json_checksum(
        all_cabal_hashes,
        resolved,
    )
    if actual_checksum != expected_checksum:
        fail(errmsg.format(
            error = "Mismatching checksum, expected {expected}, got {actual}.".format(
                expected = expected_checksum,
                actual = actual_checksum,
            ),
        ))

    return pinned

def _stack_snapshot_unpinned_impl(repository_ctx):
    snapshot = _parse_stack_snapshot(
        repository_ctx,
        repository_ctx.attr.snapshot,
        repository_ctx.attr.local_snapshot,
    )

    vendored_packages = _invert(repository_ctx.attr.vendored_packages)
    packages = _parse_packages_list(
        repository_ctx.attr.packages,
        vendored_packages,
    )

    # Enforce dependency on stack_update
    repository_ctx.read(repository_ctx.attr.stack_update)

    resolved = _resolve_packages(
        repository_ctx,
        snapshot,
        packages.core,
        packages.versioned,
        packages.unversioned,
        vendored_packages,
    )
    (all_cabal_hashes, resolved) = _pin_packages(repository_ctx, resolved)

    _write_snapshot_json(repository_ctx, all_cabal_hashes, resolved)

    repository_name = repository_ctx.name[:-len("-unpinned")]

    if repository_ctx.attr.stack_snapshot_json:
        stack_snapshot_location = paths.join(
            repository_ctx.attr.stack_snapshot_json.package,
            repository_ctx.attr.stack_snapshot_json.name,
        )
    else:
        stack_snapshot_location = "%s_snapshot.json" % repository_name

    repository_ctx.template(
        "pin.sh",
        repository_ctx.path(Label("@rules_haskell//haskell:private/stack_snapshot_pin.sh.tpl")),
        executable = True,
        substitutions = {
            "{repository_name}": repository_name,
            "{stack_snapshot_source}": "snapshot.json",
            "{stack_snapshot_location}": stack_snapshot_location,
            "{predefined_stack_snapshot}": str(repository_ctx.attr.stack_snapshot_json != None),
        },
    )

    repository_ctx.file(
        "BUILD.bazel",
        executable = False,
        content = """\
sh_binary(
    name = "pin",
    data = ["snapshot.json"],
    deps = ["@bazel_tools//tools/bash/runfiles"],
    srcs = ["pin.sh"],
)
""",
    )

def _stack_snapshot_impl(repository_ctx):
    snapshot = _parse_stack_snapshot(
        repository_ctx,
        repository_ctx.attr.snapshot,
        repository_ctx.attr.local_snapshot,
    )

    vendored_packages = _invert(repository_ctx.attr.vendored_packages)
    packages = _parse_packages_list(
        repository_ctx.attr.packages,
        vendored_packages,
    )

    # Resolve and fetch packages
    if repository_ctx.attr.stack_snapshot_json == None:
        # Enforce dependency on stack_update
        repository_ctx.read(repository_ctx.attr.stack_update)
        resolved = _resolve_packages(
            repository_ctx,
            snapshot,
            packages.core,
            packages.versioned,
            packages.unversioned,
            vendored_packages,
        )
        _download_packages_unpinned(repository_ctx, snapshot, resolved)
    else:
        pinned = _read_snapshot_json(repository_ctx, repository_ctx.attr.stack_snapshot_json)
        _download_packages(repository_ctx, snapshot, pinned)
        resolved = pinned["resolved"]

    user_components = {
        name: _parse_components(name, components)
        for (name, components) in repository_ctx.attr.components.items()
    }
    all_components = {}
    for (name, spec) in resolved.items():
        all_components[name] = _get_components(user_components, name)
        user_components.pop(name, None)
    for package in user_components.keys():
        if not package in _default_components:
            fail("Unknown package: %s" % package, "components")

    extra_deps = _to_string_keyed_label_list_dict(repository_ctx.attr.extra_deps)
    tools = [_label_to_string(label) for label in repository_ctx.attr.tools]

    # Write out dependency graph as importable Starlark value.
    repository_ctx.file(
        "packages.bzl",
        """\
packages = {
    %s
}
""" % ",\n    ".join([
            '"%s": ' % name + repr(struct(
                name = name,
                version = spec["version"],
                library = all_components[name].lib,
                executables = all_components[name].exe,
                deps = [
                    Label("@{}//:{}".format(repository_ctx.name, dep))
                    for dep in spec["dependencies"]
                    if all_components[dep].lib
                ],
                tools = [
                    Label("@{}-exe//{}:{}".format(repository_ctx.name, dep, exe))
                    for dep in spec["dependencies"]
                    for exe in all_components[dep].exe
                ],
                flags = repository_ctx.attr.flags.get(name, []),
            ))
            for (name, spec) in resolved.items()
        ]),
        executable = False,
    )

    # Write out the dependency graph as a BUILD file.
    build_file_builder = []
    build_file_builder.append("""
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary", "haskell_cabal_library")
load("@rules_haskell//haskell:defs.bzl", "haskell_library", "haskell_toolchain_library")
""")
    for (name, spec) in resolved.items():
        version = spec["version"]
        package = "%s-%s" % (name, version)
        if name in packages.all or name in vendored_packages:
            visibility = ["//visibility:public"]
        else:
            visibility = ["//visibility:private"]
        if name in vendored_packages:
            build_file_builder.append(
                """
alias(name = "{name}", actual = "{actual}", visibility = {visibility})
""".format(name = name, actual = vendored_packages[name], visibility = visibility),
            )
        elif name in _CORE_PACKAGES:
            build_file_builder.append(
                """
haskell_toolchain_library(name = "{name}", visibility = {visibility})
""".format(name = name, visibility = visibility),
            )
        elif name in _EMPTY_PACKAGES_BLACKLIST:
            build_file_builder.append(
                """
haskell_library(
    name = "{name}",
    version = "{version}",
    visibility = {visibility},
)
""".format(
                    name = name,
                    version = version,
                    visibility = visibility,
                ),
            )
        else:
            library_deps = [
                dep
                for dep in spec["dependencies"]
                if all_components[dep].lib
            ] + [
                _label_to_string(label)
                for label in extra_deps.get(name, [])
            ]
            library_tools = [
                "_%s_exe_%s" % (dep, exe)
                for dep in spec["dependencies"]
                for exe in all_components[dep].exe
            ] + tools
            setup_deps = [
                _label_to_string(Label("@{}//:{}".format(repository_ctx.name, name)).relative(label))
                for label in repository_ctx.attr.setup_deps.get(name, [])
            ]
            if all_components[name].lib:
                build_file_builder.append(
                    """
haskell_cabal_library(
    name = "{name}",
    version = "{version}",
    haddock = {haddock},
    flags = {flags},
    srcs = glob(["{dir}/**"]),
    deps = {deps},
    setup_deps = {setup_deps},
    tools = {tools},
    visibility = {visibility},
    cabalopts = ["--ghc-option=-w", "--ghc-option=-optF=-w"],
    verbose = {verbose},
    unique_name = True,
)
""".format(
                        name = name,
                        version = version,
                        haddock = repr(repository_ctx.attr.haddock),
                        flags = repository_ctx.attr.flags.get(name, []),
                        dir = package,
                        deps = library_deps,
                        setup_deps = setup_deps,
                        tools = library_tools,
                        visibility = visibility,
                        verbose = repr(repository_ctx.attr.verbose),
                    ),
                )
                build_file_builder.append(
                    """alias(name = "{name}", actual = ":{actual}", visibility = {visibility})""".format(
                        name = package,
                        actual = name,
                        visibility = visibility,
                    ),
                )
            for exe in all_components[name].exe:
                build_file_builder.append(
                    """
haskell_cabal_binary(
    name = "_{name}_exe_{exe}",
    exe_name = "{exe}",
    flags = {flags},
    srcs = glob(["{dir}/**"]),
    deps = {deps},
    tools = {tools},
    visibility = ["@{workspace}-exe//{name}:__pkg__"],
    cabalopts = ["--ghc-option=-w", "--ghc-option=-optF=-w"],
    verbose = {verbose},
)
""".format(
                        workspace = repository_ctx.name,
                        name = name,
                        exe = exe,
                        flags = repository_ctx.attr.flags.get(name, []),
                        dir = package,
                        deps = library_deps + ([name] if all_components[name].lib else []),
                        setup_deps = setup_deps,
                        tools = library_tools,
                        visibility = visibility,
                        verbose = repr(repository_ctx.attr.verbose),
                    ),
                )
    build_file_content = "\n".join(build_file_builder)
    repository_ctx.file("BUILD.bazel", build_file_content, executable = False)

_stack_snapshot_unpinned = repository_rule(
    _stack_snapshot_unpinned_impl,
    attrs = {
        "stack_snapshot_json": attr.label(allow_single_file = True),
        "snapshot": attr.string(),
        "local_snapshot": attr.label(allow_single_file = True),
        "packages": attr.string_list(),
        "vendored_packages": attr.label_keyed_string_dict(),
        "flags": attr.string_list_dict(),
        "stack": attr.label(),
        "stack_update": attr.label(),
    },
)

_stack_snapshot = repository_rule(
    _stack_snapshot_impl,
    attrs = {
        "stack_snapshot_json": attr.label(allow_single_file = True),
        "snapshot": attr.string(),
        "local_snapshot": attr.label(allow_single_file = True),
        "packages": attr.string_list(),
        "vendored_packages": attr.label_keyed_string_dict(),
        "flags": attr.string_list_dict(),
        "haddock": attr.bool(default = True),
        "extra_deps": attr.label_keyed_string_dict(),
        "setup_deps": attr.string_list_dict(),
        "tools": attr.label_list(),
        "components": attr.string_list_dict(),
        "stack": attr.label(),
        "stack_update": attr.label(),
        "verbose": attr.bool(default = False),
    },
)

def _stack_executables_impl(repository_ctx):
    workspace = repository_ctx.name[:-len("-exe")]
    packages = [
        _chop_version(package) if _has_version(package) else package
        for package in repository_ctx.attr.packages
    ]
    for package in packages:
        repository_ctx.file(package + "/BUILD.bazel", executable = False, content = """\
load("@{workspace}//:packages.bzl", "packages")
[
    alias(
        name = exe,
        actual = "@{workspace}//:_{package}_exe_" + exe,
        visibility = ["//visibility:public"],
    )
    for exe in packages["{package}"].executables
]
""".format(
            workspace = workspace,
            package = package,
        ))

_stack_executables = repository_rule(
    _stack_executables_impl,
    attrs = {
        "packages": attr.string_list(),
    },
)

def _stack_update_impl(repository_ctx):
    stack_cmd = repository_ctx.path(repository_ctx.attr.stack)
    _execute_or_fail_loudly(repository_ctx, [stack_cmd, "update"])
    repository_ctx.file("stack_update")
    repository_ctx.file("BUILD.bazel", content = "exports_files(['stack_update'])")

_stack_update = repository_rule(
    _stack_update_impl,
    attrs = {
        "stack": attr.label(),
    },
    # Marked as local so that stack update is always executed before
    # _stack_snapshot is executed.
    local = True,
)
"""Execute stack update.

This is extracted into a singleton repository rule to avoid concurrent
invocations of stack update.
See https://github.com/tweag/rules_haskell/issues/1090
"""

def _get_platform(repository_ctx):
    """Map OS name and architecture to Stack platform identifiers."""
    os_name = repository_ctx.os.name.lower()
    if os_name.startswith("linux"):
        os = "linux"
    elif os_name.startswith("mac os"):
        os = "osx"
    elif os_name.find("freebsd") != -1:
        os = "freebsd"
    elif os_name.find("windows") != -1:
        os = "windows"
    else:
        fail("Unknown OS: '{}'".format(os_name))

    if os == "windows":
        reg_query = ["reg", "QUERY", "HKLM\\SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment", "/v", "PROCESSOR_ARCHITECTURE"]
        result = repository_ctx.execute(reg_query)
        value = result.stdout.strip().split(" ")[-1].lower()
        if value in ["amd64", "ia64"]:
            arch = "x86_64"
        elif value in ["x86"]:
            arch = "i386"
        else:
            fail("Failed to determine CPU architecture:\n{}\n{}".format(result.stdout, result.stderr))
    else:
        result = repository_ctx.execute(["uname", "-m"])
        if result.stdout.strip() in ["arm", "armv7l"]:
            arch = "arm"
        elif result.stdout.strip() in ["aarch64"]:
            arch = "aarch64"
        elif result.stdout.strip() in ["amd64", "x86_64", "x64"]:
            arch = "x86_64"
        elif result.stdout.strip() in ["i386", "i486", "i586", "i686"]:
            arch = "i386"
        else:
            fail("Failed to determine CPU architecture:\n{}\n{}".format(result.stdout, result.stderr))

    return (os, arch)

def _fetch_stack_impl(repository_ctx):
    repository_ctx.file("BUILD.bazel")
    stack_cmd = repository_ctx.which("stack")
    if stack_cmd:
        if _stack_version_check(repository_ctx, stack_cmd):
            repository_ctx.symlink(stack_cmd, "stack")
            return
        else:
            print("Stack version not recent enough. Downloading a newer version...")

    # If we can't find Stack, download it.
    (os, arch) = _get_platform(repository_ctx)
    version = _STACK_DEFAULT_VERSION
    (url, sha256) = _STACK_BINDISTS[version]["{}-{}".format(os, arch)]
    prefix = paths.basename(url)[:-len(".tar.gz")]
    repository_ctx.download_and_extract(url = url, sha256 = sha256)
    stack_cmd = repository_ctx.path(prefix).get_child("stack.exe" if os == "windows" else "stack")
    _execute_or_fail_loudly(repository_ctx, [stack_cmd, "--version"])
    exec_result = repository_ctx.execute([stack_cmd, "--version"], quiet = True)
    if exec_result.return_code != 0:
        error_messsage = ["A Stack binary for your platform exists, but it failed to execute."]
        if os == "linux":
            error_messsage.append("HINT: If you are on NixOS,")
            error_messsage.append("* make Stack available on the PATH, or")
            error_messsage.append("* specify a Stack binary using the stack attribute.")
        fail("\n".join(error_messsage).format(exec_result.return_code))
    repository_ctx.symlink(stack_cmd, "stack")

_fetch_stack = repository_rule(
    _fetch_stack_impl,
)
"""Find a suitably recent local Stack or download it."""

def stack_snapshot(
        name,
        stack = None,
        extra_deps = {},
        vendored_packages = {},
        snapshot = "",
        local_snapshot = None,
        stack_snapshot_json = None,
        packages = [],
        flags = {},
        haddock = True,
        setup_deps = {},
        tools = [],
        components = {},
        stack_update = None,
        verbose = False,
        **kwargs):
    """Use Stack to download and extract Cabal source distributions.

    This rule will use Stack to compute the transitive closure of the
    subset of the given snapshot listed in the `packages` attribute, and
    generate a dependency graph. If a package in the closure depends on
    system libraries or other external libraries, use the `extra_deps`
    attribute to list them. This attribute works like the
    `--extra-{include,lib}-dirs` flags for Stack and cabal-install do.
    If a package has a custom setup with setup dependencies, use the
    `setup_deps` attribute to list them.

    Packages that are in the snapshot need not have their versions
    specified. But any additional packages or version overrides will have
    to be specified with a package identifier of the form
    `<package>-<version>` in the `packages` attribute. Note that you cannot
    override the version of any [packages built into GHC][ghc-builtins].

    This rule invokes the `stack` tool for version and dependency resolution
    based on the specified snapshot. You can generate a `stack_snapshot.json`
    file to avoid invoking `stack` on every fetch and instead pin the outcome
    in a file that can be checked into revision control. Execute the following
    command:

    ```
    bazel run @stackage-unpinned//:pin
    ```

    Then specify the `stack_snapshot_json` attribute to point to the generated
    file:

    ```
    stack_snapshot(
        ...
        stack_snapshot_json = "//:stackage_snapshot.json",
    )
    ```

    By default `stack_snapshot` defines a library target for each package. If a
    package does not contain a library component or contains executable
    components, then you need to declare so yourself using the `components`
    attribute. Library targets are exposed as `@stackage//:<package-name>` and
    executables are exposed as
    `@stackage-exe//<package-name>:<executable-name>`, assuming that you
    invoked `stack_snapshot` with `name = "stackage"`.

    In the external repository defined by the rule, all given packages are
    available as top-level targets named after each package. Additionally, the
    dependency graph is made available within `packages.bzl` as the `dict`
    `packages` mapping unversioned package names to structs holding the fields

      - name: The unversioned package name.
      - version: The package version.
      - library: Whether the package has a declared library component.
      - executables: List of declared executable components.
      - deps: The list of library dependencies according to stack.
      - tools: The list of tool dependencies according to stack.
      - flags: The list of Cabal flags.

    **NOTE:** Make sure your GHC version matches the version expected by the
    snapshot. E.g. if you pass `snapshot = "lts-13.15"`, make sure you use
    GHC 8.6.4 (e.g. by invoking `rules_haskell_toolchains(version="8.6.4")`).
    Sadly, rules_haskell cannot maintain this correspondence for you. You will
    need to manage it yourself. If you have a version mismatch, you will end up
    with versions of [core GHC packages][ghc-builtins] which do not match the
    versions listed in the snapshot, and potentially other problems.

    [ghc-builtins]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/8.10.1-notes.html#included-libraries

    ### Examples

      ```bzl
      stack_snapshot(
          name = "stackage",
          packages = ["conduit", "doctest", "lens", "zlib-0.6.2"],
          vendored_packages = {"split": "//split:split"},
          tools = ["@happy//:happy"],  # Use externally provided `happy`
          components = {
              "doctest": ["lib", "exe"],  # Optional since doctest is known to have an exe component.
              "happy": [],  # Override happy's default exe component.
          },
          snapshot = "lts-13.15",
          extra_deps = {"zlib": ["@zlib.dev//:zlib"]},
      )
      ```
      defines `@stackage//:conduit`, `@stackage//:doctest`, `@stackage//:lens`,
      `@stackage//:zlib` library targets and a `@stackage-exe//doctest`
      executable target. It also uses an externally provided `happy` rather
      than the one provided by the snapshot.

      Alternatively

      ```bzl
      stack_snapshot(
          name = "stackage",
          packages = ["conduit", "doctest", "lens", "zlib"],
          flags = {"zlib": ["-non-blocking-ffi"]},
          tools = ["@happy//:happy"],  # Use externally provided `happy`
          components = {
              "doctest": ["lib", "exe"],  # Optional since doctest is known to have an exe component.
              "happy": [],  # Override happy's default exe component.
          },
          local_snapshot = "//:snapshot.yaml",
          extra_deps = {"zlib": ["@zlib.dev//:zlib"]},
      ```

      Does the same as the previous example, provided there is a
      `snapshot.yaml`, at the root of the repository with content

      ```yaml
      resolver: lts-13.15

      packages:
        - zlib-0.6.2
      ```

    Args:
      name: The name of the Bazel workspace.
      snapshot: The name of a Stackage snapshot. Incompatible with local_snapshot.
      local_snapshot: A custom Stack snapshot file, as per the Stack documentation.
        Incompatible with snapshot.
      stack_snapshot_json: A label to a `stack_snapshot.json` file, e.g. `//:stack_snapshot.json`.
        Specify this to use pinned artifacts for generating build targets.
      packages: A set of package identifiers. For packages in the snapshot,
        version numbers can be omitted.
      vendored_packages: Add or override a package to the snapshot with a custom
        unpacked source distribution. Each package must contain a Cabal file
        named `<package-name>.cabal` in the package root.
      flags: A dict from package name to list of flags.
      extra_deps: Extra dependencies of packages, e.g. system libraries or C/C++ libraries.
        Dict of stackage package names to a list of targets. The list of targets is given
        as input to the named stackage package.
        ```
        {
            "postgresql-libpq": ["@postgresql//:include"],
            "zlib": ["@zlib.dev//:zlib"]
        }
        ```
        means `@postgresql//:include` is passed to the stackage package `postgresql-libpq`
        while `@zlib.dev//:zlib` is passed to the stackage package `zlib`.
      setup_deps: Setup dependencies of packages, e.g. `cabal-doctest`.
        Dict of stackage package names to a list of targets in the same format as for `extra_deps`.
      tools: Tool dependencies. They are built using the host configuration, since
        the tools are executed as part of the build.
      components: Defines which Cabal components to build for each package.
        A dict from package name to list of components. Use `lib` for the
        library component and `exe:<exe-name>` for an executable component,
        `exe` is a short-cut for `exe:<package-name>`. The library component
        will have the label `@<workspace>//:<package>` and an executable
        component will have the label `@<workspace>-exe//<package>:<exe-name>`,
        where `<workspace>` is the name given to the `stack_snapshot`
        invocation.
      stack: The stack binary to use to enumerate package dependencies.
      haddock: Whether to generate haddock documentation.
      verbose: Whether to show the output of the build.
      stack_update: A meta repository that is used to avoid multiple concurrent invocations of
        `stack update` which could fail due to a race on the hackage security lock.
    """
    typecheck_stackage_extradeps(extra_deps)
    if not stack:
        _fetch_stack(name = "rules_haskell_stack")
        stack = Label("@rules_haskell_stack//:stack")

    # Execute stack update once before executing _stack_snapshot.
    # This is to avoid multiple concurrent executions of stack update,
    # which may fail due to ~/.stack/pantry/hackage/hackage-security-lock.
    # See https://github.com/tweag/rules_haskell/issues/1090.
    maybe(
        _stack_update,
        name = "rules_haskell_stack_update",
        stack = stack,
    )
    _stack_snapshot_unpinned(
        name = name + "-unpinned",
        stack = stack,
        # Dependency for ordered execution, stack update before stack unpack.
        stack_update = "@rules_haskell_stack_update//:stack_update",
        vendored_packages = _invert(vendored_packages),
        snapshot = snapshot,
        local_snapshot = local_snapshot,
        stack_snapshot_json = stack_snapshot_json,
        packages = packages,
        flags = flags,
    )
    _stack_snapshot(
        name = name,
        stack = stack,
        # Dependency for ordered execution, stack update before stack unpack.
        stack_update = None if stack_snapshot_json else "@rules_haskell_stack_update//:stack_update",
        # TODO Remove _from_string_keyed_label_list_dict once following issue
        # is resolved: https://github.com/bazelbuild/bazel/issues/7989.
        extra_deps = _from_string_keyed_label_list_dict(extra_deps),
        # TODO Remove _invert once following issue is resolved:
        # https://github.com/bazelbuild/bazel/issues/7989.
        vendored_packages = _invert(vendored_packages),
        snapshot = snapshot,
        local_snapshot = local_snapshot,
        stack_snapshot_json = stack_snapshot_json,
        packages = packages,
        flags = flags,
        haddock = haddock,
        setup_deps = setup_deps,
        tools = tools,
        components = components,
        verbose = verbose,
        **kwargs
    )
    _stack_executables(
        name = name + "-exe",
        packages = packages,
    )

def _expand_make_variables(name, ctx, strings):
    extra_label_attrs = [
        ctx.attr.srcs,
        ctx.attr.tools,
    ]
    return expand_make_variables(name, ctx, strings, extra_label_attrs)
