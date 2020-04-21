"""Cabal packages"""

load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
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
_CABAL_TOOL_LIBRARIES = ["cpphs", "doctest"]

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
        srcs,
        compiler_flags,
        flags,
        generate_haddock,
        cabal_wrapper,
        package_database,
        verbose,
        dynamic_binary = None):
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
    # RUNPATH flags for all dynamic C library dependencies.
    (_, dynamic_libs) = get_library_files(
        hs,
        cc.cc_libraries_info,
        get_cc_libraries(cc.cc_libraries_info, cc.transitive_libraries),
        dynamic = True,
    )

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
    args.add("--flags=" + " ".join(flags))
    args.add_all(compiler_flags, format_each = "--ghc-option=%s")
    if dynamic_binary:
        args.add_all(
            [
                "--ghc-option=-optl-Wl,-rpath," + create_rpath_entry(
                    binary = dynamic_binary,
                    dependency = lib,
                    keep_filename = False,
                    prefix = relative_rpath_prefix(hs.toolchain.is_darwin),
                )
                for lib in dynamic_libs
            ],
            uniquify = True,
        )
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
            transitive_headers,
            depset(transitive_compile_libs),
            depset(transitive_link_libs),
            dep_info.interface_dirs,
            dep_info.static_libraries,
            dep_info.dynamic_libraries,
            tool_inputs,
        ],
    )
    input_manifests = tool_input_manifests

    return struct(
        cabal_wrapper = cabal_wrapper,
        args = args,
        inputs = inputs,
        input_manifests = input_manifests,
        env = env,
        runfiles = depset(direct = dynamic_libs),
    )

def _haskell_cabal_library_impl(ctx):
    hs = haskell_context(ctx)
    dep_info = gather_dep_info(ctx, ctx.attr.deps)
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

    user_compile_flags = _expand_make_variables("compiler_flags", ctx, ctx.attr.compiler_flags)
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
    static_library_filename = "_install/lib/libHS{}.a".format(package_id)
    if with_profiling:
        static_library_filename = "_install/lib/libHS{}_p.a".format(package_id)
    static_library = hs.actions.declare_file(
        static_library_filename,
        sibling = cabal,
    )
    if hs.toolchain.is_static:
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
        srcs = ctx.files.srcs,
        compiler_flags = user_compile_flags,
        flags = ctx.attr.flags,
        generate_haddock = ctx.attr.haddock,
        cabal_wrapper = ctx.executable._cabal_wrapper,
        package_database = package_database,
        verbose = ctx.attr.verbose,
        dynamic_binary = dynamic_library,
    )
    outputs = [
        package_database,
        interfaces_dir,
        static_library,
        data_dir,
    ]
    if ctx.attr.haddock:
        outputs.extend([haddock_file, haddock_html_dir])
    if dynamic_library != None:
        outputs.append(dynamic_library)
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
        static_libraries = depset(
            direct = [static_library],
            transitive = [dep_info.static_libraries],
            order = "topological",
        ),
        dynamic_libraries = depset(
            direct = [dynamic_library] if dynamic_library != None else [],
            transitive = [dep_info.dynamic_libraries],
        ),
        interface_dirs = depset([interfaces_dir], transitive = [dep_info.interface_dirs]),
        compile_flags = [],
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
    library_to_link = cc_common.create_library_to_link(
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        dynamic_library = dynamic_library,
        static_library = static_library,
        cc_toolchain = cc_toolchain,
    )
    compilation_context = cc_common.create_compilation_context()
    linking_context = cc_common.create_linking_context(
        libraries_to_link = [library_to_link],
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
        "srcs": attr.label_list(allow_files = True),
        "deps": attr.label_list(
            aspects = [haskell_cc_libraries_aspect],
        ),
        "compiler_flags": attr.string_list(
            doc = """Flags to pass to Haskell compiler, in addition to those defined
            the cabal file. Subject to Make variable substitution.""",
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

    user_compile_flags = _expand_make_variables("compiler_flags", ctx, ctx.attr.compiler_flags)
    cabal = _find_cabal(hs, ctx.files.srcs)
    setup = _find_setup(hs, cabal, ctx.files.srcs)
    package_database = hs.actions.declare_file(
        "_install/{}.conf.d/package.cache".format(hs.label.name),
        sibling = cabal,
    )
    binary = hs.actions.declare_file(
        "_install/bin/{name}{ext}".format(
            name = hs.label.name,
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
        component = "exe:{}".format(hs.label.name),
        package_id = hs.label.name,
        tool_inputs = tool_inputs,
        tool_input_manifests = tool_input_manifests,
        cabal = cabal,
        setup = setup,
        srcs = ctx.files.srcs,
        compiler_flags = user_compile_flags,
        flags = ctx.attr.flags,
        generate_haddock = False,
        cabal_wrapper = ctx.executable._cabal_wrapper,
        package_database = package_database,
        verbose = ctx.attr.verbose,
        dynamic_binary = binary,
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
        static_libraries = dep_info.static_libraries,
        dynamic_libraries = dep_info.dynamic_libraries,
        interface_dirs = dep_info.interface_dirs,
        compile_flags = [],
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
        "srcs": attr.label_list(allow_files = True),
        "deps": attr.label_list(
            aspects = [haskell_cc_libraries_aspect],
        ),
        "compiler_flags": attr.string_list(
            doc = """Flags to pass to Haskell compiler, in addition to those defined
            the cabal file. Subject to Make variable substitution.""",
        ),
        "tools": attr.label_list(
            cfg = "host",
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

_STACK_DEFAULT_VERSION = "2.1.3"

# Only ever need one version, but use same structure as for GHC bindists.
_STACK_BINDISTS = \
    {
        "2.1.3": {
            "freebsd-x86_64": (
                "https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-freebsd-x86_64.tar.gz",
                "b646380bd1ee6c5f16ea111c31be494e6e85ed5050dea41cd29fac5973767821",
            ),
            "linux-aarch64": (
                "https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-linux-aarch64.tar.gz",
                "1212c3ef9c4e901c50b086f1d778c28d75eb27cb4529695d2f1a16ea3f898a6d",
            ),
            "linux-arm": (
                "https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-linux-arm.tar.gz",
                "6c8a2100183368d0fe8298bc99260681f10c81838423884be885baaa2e096e78",
            ),
            "linux-i386": (
                "https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-linux-i386.tar.gz",
                "4acd97f4c91b1d1333c8d84ea38f690f0b5ac5224ba591f8cdd1b9d0e8973807",
            ),
            "linux-x86_64": (
                "https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-linux-x86_64.tar.gz",
                "c724b207831fe5f06b087bac7e01d33e61a1c9cad6be0468f9c117d383ec5673",
            ),
            "osx-x86_64": (
                "https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-osx-x86_64.tar.gz",
                "84b05b9cdb280fbc4b3d5fe23d1fc82a468956c917e16af7eeeabec5e5815d9f",
            ),
            "windows-i386": (
                "https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-windows-i386.tar.gz",
                "9bc67a8dc0466b6fc12b44b3920ea6be3b00fa1c52cbeada1a7c092a5402ebb3",
            ),
            "windows-x86_64": (
                "https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-windows-x86_64.tar.gz",
                "075bcd9130cd437de4e726466e5738c92c8e47d5666aa3a15d339e6ba62f76b2",
            ),
        },
    }

def _stack_version_check(repository_ctx, stack_cmd):
    """Returns False if version not recent enough."""
    exec_result = _execute_or_fail_loudly(repository_ctx, [stack_cmd, "--numeric-version"])

    stack_major_version = int(exec_result.stdout.split(".")[0])
    return stack_major_version >= 2

def _compute_dependency_graph(repository_ctx, snapshot, core_packages, versioned_packages, unversioned_packages, vendored_packages):
    """Given a list of root packages, compute a dependency graph.

    Returns:
      dict(name: struct(name, version, versioned_name, deps, is_core_package, sdist)):
        name: The unversioned package name.
        version: The version of the package.
        versioned_name: <name>-<version>.
        flags: Cabal flags for this package.
        deps: The list of dependencies.
        vendored: Label of vendored package, None if not vendored.
        is_core_package: Whether the package is a core package.
        sdist: directory name of the unpackaged source distribution or None if core package or vendored.

    """

    all_packages = {}
    for core_package in core_packages:
        all_packages[core_package] = struct(
            name = core_package,
            version = None,
            versioned_name = None,
            flags = repository_ctx.attr.flags.get(core_package, []),
            deps = [],
            vendored = None,
            is_core_package = True,
            sdist = None,
        )

    if not versioned_packages and not unversioned_packages and not vendored_packages:
        return all_packages

    # Unpack all given packages, then compute the transitive closure
    # and unpack anything in the transitive closure as well.
    stack_cmd = repository_ctx.path(repository_ctx.attr.stack)
    if not _stack_version_check(repository_ctx, stack_cmd):
        fail("Stack version not recent enough. Need version 2.1 or newer.")
    stack = [stack_cmd]

    if versioned_packages:
        _execute_or_fail_loudly(repository_ctx, stack + ["unpack"] + versioned_packages)
    stack = [stack_cmd, "--resolver", snapshot]
    if unversioned_packages:
        _execute_or_fail_loudly(repository_ctx, stack + ["unpack"] + unversioned_packages)
    exec_result = _execute_or_fail_loudly(repository_ctx, ["ls"])
    unpacked_sdists = exec_result.stdout.splitlines()

    # Determines path to vendored package's root directory relative to stack.yaml.
    # Note, this requires that the Cabal file exists in the package root and is
    # called `<name>.cabal`.
    vendored_sdists = [
        truly_relativize(
            str(repository_ctx.path(label.relative(name + ".cabal")).dirname),
            relative_to = str(repository_ctx.path("stack.yaml").dirname),
        )
        for (name, label) in vendored_packages.items()
    ]
    package_flags = {
        pkg_name: {
            flag[1:] if flag.startswith("-") else flag: not flag.startswith("-")
            for flag in flags
        }
        for (pkg_name, flags) in repository_ctx.attr.flags.items()
    }
    stack_yaml_content = struct(resolver = "none", packages = unpacked_sdists + vendored_sdists, flags = package_flags).to_json()
    repository_ctx.file("stack.yaml", content = stack_yaml_content, executable = False)
    exec_result = _execute_or_fail_loudly(
        repository_ctx,
        stack + ["ls", "dependencies", "--global-hints", "--separator=-"],
    )
    transitive_unpacked_sdists = []
    indirect_unpacked_sdists = []
    for package in exec_result.stdout.splitlines():
        name = _chop_version(package)
        if name in _CABAL_TOOLS and not name in _CABAL_TOOL_LIBRARIES:
            continue

        version = _version(package)
        vendored = vendored_packages.get(name, None)
        is_core_package = name in _CORE_PACKAGES
        all_packages[name] = struct(
            name = name,
            version = version,
            versioned_name = package,
            flags = repository_ctx.attr.flags.get(name, []),
            deps = [],
            vendored = vendored,
            is_core_package = is_core_package,
            sdist = None if is_core_package or vendored != None else package,
        )

        if is_core_package or vendored != None:
            continue

        if version == "<unknown>":
            fail("""\
Could not resolve version of {}. It is not in the snapshot.
Specify a fully qualified package name of the form <package>-<version>.
            """.format(package))

        transitive_unpacked_sdists.append(package)
        if package not in unpacked_sdists:
            indirect_unpacked_sdists.append(name)

    # We removed the version numbers prior to calling `unpack`. This
    # way, stack will fetch the package sources from the snapshot
    # rather than from Hackage. See #1027.
    if indirect_unpacked_sdists:
        _execute_or_fail_loudly(repository_ctx, stack + ["unpack"] + indirect_unpacked_sdists)
    stack_yaml_content = struct(resolver = "none", packages = transitive_unpacked_sdists + vendored_sdists, flags = package_flags).to_json()
    repository_ctx.file("stack.yaml", stack_yaml_content, executable = False)

    # Compute dependency graph.
    exec_result = _execute_or_fail_loudly(
        repository_ctx,
        stack + ["dot", "--global-hints", "--external"],
    )
    for line in exec_result.stdout.splitlines():
        tokens = [w.strip('";') for w in line.split(" ")]

        # All lines of the form `"foo" -> "bar";` declare edges of the
        # dependency graph in the Graphviz format.
        if len(tokens) == 3 and tokens[1] == "->":
            [src, _, dest] = tokens
            if src in all_packages and dest in all_packages:
                all_packages[src].deps.append(dest)
    return all_packages

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

def _stack_snapshot_impl(repository_ctx):
    if repository_ctx.attr.snapshot and repository_ctx.attr.local_snapshot:
        fail("Please specify either snapshot or local_snapshot, but not both.")
    elif repository_ctx.attr.snapshot:
        snapshot = repository_ctx.attr.snapshot
    elif repository_ctx.attr.local_snapshot:
        snapshot = repository_ctx.path(repository_ctx.attr.local_snapshot)
    else:
        fail("Please specify one of snapshot or repository_snapshot")

    # Enforce dependency on stack_update
    repository_ctx.read(repository_ctx.attr.stack_update)

    vendored_packages = _invert(repository_ctx.attr.vendored_packages)
    packages = repository_ctx.attr.packages
    core_packages = []
    versioned_packages = []
    unversioned_packages = []
    for package in packages:
        has_version = _has_version(package)
        unversioned = _chop_version(package) if has_version else package
        if unversioned in vendored_packages:
            fail("Duplicate package '{}'. Packages may not be listed in both 'packages' and 'vendored_packages'.".format(package))
        if unversioned in _CORE_PACKAGES:
            core_packages.append(unversioned)
        elif has_version:
            versioned_packages.append(package)
        else:
            unversioned_packages.append(package)
    all_packages = _compute_dependency_graph(
        repository_ctx,
        snapshot,
        core_packages,
        versioned_packages,
        unversioned_packages,
        vendored_packages,
    )

    extra_deps = _to_string_keyed_label_list_dict(repository_ctx.attr.extra_deps)
    tools = [_label_to_string(label) for label in repository_ctx.attr.tools]

    # Write out dependency graph as importable Starlark value.
    repository_ctx.file(
        "packages.bzl",
        "packages = " + repr({
            package.name: struct(
                name = package.name,
                version = package.version,
                deps = [Label("@{}//:{}".format(repository_ctx.name, dep)) for dep in package.deps],
                flags = package.flags,
            )
            for package in all_packages.values()
        }),
        executable = False,
    )

    # Write out the dependency graph as a BUILD file.
    build_file_builder = []
    build_file_builder.append("""
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@rules_haskell//haskell:defs.bzl", "haskell_library", "haskell_toolchain_library")
""")
    for package in all_packages.values():
        if package.name in packages or package.versioned_name in packages or package.vendored != None:
            visibility = ["//visibility:public"]
        else:
            visibility = ["//visibility:private"]
        if package.vendored != None:
            build_file_builder.append(
                """
alias(name = "{name}", actual = "{actual}", visibility = {visibility})
""".format(name = package.name, actual = package.vendored, visibility = visibility),
            )
        elif package.is_core_package:
            build_file_builder.append(
                """
haskell_toolchain_library(name = "{name}", visibility = {visibility})
""".format(name = package.name, visibility = visibility),
            )
        elif package.name in _EMPTY_PACKAGES_BLACKLIST:
            build_file_builder.append(
                """
haskell_library(
    name = "{name}",
    version = "{version}",
    visibility = {visibility},
)
""".format(
                    name = package.name,
                    version = package.version,
                    visibility = visibility,
                ),
            )
        else:
            build_file_builder.append(
                """
haskell_cabal_library(
    name = "{name}",
    version = "{version}",
    haddock = {haddock},
    flags = {flags},
    srcs = glob(["{dir}/**"]),
    deps = {deps},
    tools = {tools},
    visibility = {visibility},
    compiler_flags = ["-w", "-optF=-w"],
    verbose = {verbose},
)
""".format(
                    name = package.name,
                    version = package.version,
                    haddock = repr(repository_ctx.attr.haddock),
                    flags = package.flags,
                    dir = package.sdist,
                    deps = package.deps + [
                        _label_to_string(label)
                        for label in extra_deps.get(package.name, [])
                    ],
                    tools = tools,
                    visibility = visibility,
                    verbose = repr(repository_ctx.attr.verbose),
                ),
            )
        if package.versioned_name != None:
            build_file_builder.append(
                """alias(name = "{name}", actual = ":{actual}", visibility = {visibility})""".format(
                    name = package.versioned_name,
                    actual = package.name,
                    visibility = visibility,
                ),
            )
    build_file_content = "\n".join(build_file_builder)
    repository_ctx.file("BUILD.bazel", build_file_content, executable = False)

_stack_snapshot = repository_rule(
    _stack_snapshot_impl,
    attrs = {
        "snapshot": attr.string(),
        "local_snapshot": attr.label(allow_single_file = True),
        "packages": attr.string_list(),
        "vendored_packages": attr.label_keyed_string_dict(),
        "flags": attr.string_list_dict(),
        "haddock": attr.bool(
            default = True,
            doc = "Whether to generate haddock documentation",
        ),
        "extra_deps": attr.label_keyed_string_dict(),
        "tools": attr.label_list(),
        "stack": attr.label(),
        "stack_update": attr.label(),
        "verbose": attr.bool(
            default = False,
            doc = "Whether to show the output of the build",
        ),
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
    repository_ctx.download_and_extract(url = url, sha256 = sha256)
    stack_cmd = repository_ctx.path(
        "stack-{}-{}-{}".format(version, os, arch),
    ).get_child("stack.exe" if os == "windows" else "stack")
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

def stack_snapshot(stack = None, extra_deps = {}, vendored_packages = {}, **kwargs):
    """Use Stack to download and extract Cabal source distributions.

    This rule will use Stack to compute the transitive closure of the
    subset of the given snapshot listed in the `packages` attribute, and
    generate a dependency graph. If a package in the closure depends on
    system libraries or other external libraries, use the `extra_deps`
    attribute to list them. This attribute works like the
    `--extra-{include,lib}-dirs` flags for Stack and cabal-install do.

    Packages that are in the snapshot need not have their versions
    specified. But any additional packages or version overrides will have
    to be specified with a package identifier of the form
    `<package>-<version>` in the `packages` attribute.

    In the external repository defined by the rule, all given packages are
    available as top-level targets named after each package. Additionally, the
    dependency graph is made available within `packages.bzl` as the `dict`
    `packages` mapping unversioned package names to structs holding the fields
      - name: The unversioned package name.
      - version: The package version.
      - deps: The list of package dependencies according to stack.
      - flags: The list of Cabal flags.

    ### Examples

      ```bzl
      stack_snapshot(
          name = "stackage",
          packages = ["conduit", "lens", "zlib-0.6.2"],
          vendored_packages = {"split": "//split:split"},
          tools = ["@happy//:happy", "@c2hs//:c2hs"],
          snapshot = "lts-13.15",
          extra_deps = {"zlib": ["@zlib.dev//:zlib"]},
      )
      ```
      defines `@stackage//:conduit`, `@stackage//:lens`,
      `@stackage//:zlib` library targets.

      Alternatively

      ```bzl
      stack_snapshot(
          name = "stackage",
          packages = ["conduit", "lens", "zlib"],
          flags = {"zlib": ["-non-blocking-ffi"]},
          tools = ["@happy//:happy", "@c2hs//:c2hs"],
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
      snapshot: The name of a Stackage snapshot. Incompatible with local_snapshot.
      local_snapshot: A custom Stack snapshot file, as per the Stack documentation.
        Incompatible with snapshot.
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
      tools: Tool dependencies. They are built using the host configuration, since
        the tools are executed as part of the build.
      stack: The stack binary to use to enumerate package dependencies.
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
    _stack_snapshot(
        stack = stack,
        # Dependency for ordered execution, stack update before stack unpack.
        stack_update = "@rules_haskell_stack_update//:stack_update",
        # TODO Remove _from_string_keyed_label_list_dict once following issue
        # is resolved: https://github.com/bazelbuild/bazel/issues/7989.
        extra_deps = _from_string_keyed_label_list_dict(extra_deps),
        # TODO Remove _invert once following issue is resolved:
        # https://github.com/bazelbuild/bazel/issues/7989.
        vendored_packages = _invert(vendored_packages),
        **kwargs
    )

def _expand_make_variables(name, ctx, strings):
    extra_label_attrs = [
        ctx.attr.srcs,
        ctx.attr.tools,
    ]
    return expand_make_variables(name, ctx, strings, extra_label_attrs)
