"""Cabal packages"""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")
load(":cc.bzl", "cc_interop_info")
load(":private/context.bzl", "haskell_context", "render_env")
load(":private/dependencies.bzl", "gather_dep_info")
load(":private/mode.bzl", "is_profiling_enabled")
load(":private/set.bzl", "set")
load(
    ":private/workspace_utils.bzl",
    _execute_or_fail_loudly = "execute_or_fail_loudly",
)
load(
    ":providers.bzl",
    "HaskellInfo",
    "HaskellLibraryInfo",
    "get_ghci_extra_libs",
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

def _cabal_tool_flag(tool):
    """Return a --with-PROG=PATH flag if input is a recognized Cabal tool. None otherwise."""
    if tool.basename in _CABAL_TOOLS:
        return "--with-{}={}".format(tool.basename, tool.path)

def _prepare_cabal_inputs(hs, cc, dep_info, cc_info, tool_inputs, tool_input_manifests, cabal, setup, srcs, cabal_wrapper_tpl, package_database):
    """Compute Cabal wrapper, arguments, inputs."""
    name = hs.label.name
    with_profiling = is_profiling_enabled(hs)

    (ghci_extra_libs, env) = get_ghci_extra_libs(hs, cc_info)

    # TODO Instantiating this template could be done just once in the
    # toolchain rule.
    cabal_wrapper = hs.actions.declare_file("cabal_wrapper-{}.sh".format(hs.label.name))
    hs.actions.expand_template(
        template = cabal_wrapper_tpl,
        output = cabal_wrapper,
        is_executable = True,
        substitutions = {
            "%{ghc}": hs.tools.ghc.path,
            "%{ghc_pkg}": hs.tools.ghc_pkg.path,
            "%{runghc}": hs.tools.runghc.path,
            "%{ar}": cc.tools.ar,
            "%{strip}": cc.tools.strip,
            # XXX Workaround
            # https://github.com/bazelbuild/bazel/issues/5980.
            "%{env}": render_env(env),
        },
    )

    args = hs.actions.args()
    package_databases = dep_info.package_databases
    extra_headers = cc_info.compilation_context.headers
    extra_include_dirs = cc_info.compilation_context.includes
    extra_lib_dirs = [file.dirname for file in ghci_extra_libs.to_list()]
    args.add_all([name, setup, cabal.dirname, package_database.dirname])
    args.add_all(package_databases, map_each = _dirname, format_each = "--package-db=%s")
    args.add_all(extra_include_dirs, format_each = "--extra-include-dirs=%s")
    args.add_all(extra_lib_dirs, format_each = "--extra-lib-dirs=%s", uniquify = True)
    if with_profiling:
        args.add("--enable-profiling")
    args.add_all(tool_inputs, map_each = _cabal_tool_flag)

    inputs = depset(
        [setup, hs.tools.ghc, hs.tools.ghc_pkg, hs.tools.runghc],
        transitive = [
            depset(srcs),
            package_databases,
            extra_headers,
            ghci_extra_libs,
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
    )

def _haskell_cabal_library_impl(ctx):
    hs = haskell_context(ctx)
    dep_info = gather_dep_info(ctx, ctx.attr.deps)
    cc = cc_interop_info(ctx)
    cc_info = cc_common.merge_cc_infos(
        cc_infos = [dep[CcInfo] for dep in ctx.attr.deps if CcInfo in dep],
    )
    name = hs.label.name
    with_profiling = is_profiling_enabled(hs)

    cabal = _find_cabal(hs, ctx.files.srcs)
    setup = _find_setup(hs, cabal, ctx.files.srcs)
    package_database = hs.actions.declare_file(
        "_install/package.conf.d/package.cache",
        sibling = cabal,
    )
    interfaces_dir = hs.actions.declare_directory(
        "_install/iface",
        sibling = cabal,
    )
    data_dir = hs.actions.declare_directory(
        "_install/data",
        sibling = cabal,
    )
    static_library_filename = "_install/lib/libHS{}.a".format(name)
    if with_profiling:
        static_library_filename = "_install/lib/libHS{}_p.a".format(name)
    static_library = hs.actions.declare_file(
        static_library_filename,
        sibling = cabal,
    )
    if hs.toolchain.is_static:
        dynamic_library = None
    else:
        dynamic_library = hs.actions.declare_file(
            "_install/lib/libHS{}-ghc{}.{}".format(name, hs.toolchain.version, _so_extension(hs)),
            sibling = cabal,
        )
    (tool_inputs, tool_input_manifests) = ctx.resolve_tools(tools = ctx.attr.tools)
    c = _prepare_cabal_inputs(
        hs,
        cc,
        dep_info,
        cc_info,
        tool_inputs = tool_inputs,
        tool_input_manifests = tool_input_manifests,
        cabal = cabal,
        setup = setup,
        srcs = ctx.files.srcs,
        cabal_wrapper_tpl = ctx.file._cabal_wrapper_tpl,
        package_database = package_database,
    )
    ctx.actions.run(
        executable = c.cabal_wrapper,
        arguments = [c.args],
        inputs = c.inputs,
        input_manifests = c.input_manifests,
        outputs = [
            package_database,
            interfaces_dir,
            static_library,
            data_dir,
        ] + ([dynamic_library] if dynamic_library != None else []),
        env = c.env,
        mnemonic = "HaskellCabalLibrary",
        progress_message = "HaskellCabalLibrary {}".format(hs.label),
        use_default_shell_env = True,
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
        source_files = set.empty(),
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
    lib_info = HaskellLibraryInfo(package_id = name, version = None)
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
    return [default_info, hs_info, cc_info, lib_info]

haskell_cabal_library = rule(
    _haskell_cabal_library_impl,
    attrs = {
        "srcs": attr.label_list(allow_files = True),
        "deps": attr.label_list(),
        "tools": attr.label_list(
            cfg = "host",
            allow_files = True,
            doc = """Tool dependencies. They are built using the host configuration, since
            the tools are executed as part of the build.""",
        ),
        "_cabal_wrapper_tpl": attr.label(
            allow_single_file = True,
            default = Label("@io_tweag_rules_haskell//haskell:private/cabal_wrapper.sh.tpl"),
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
    fragments = ["cpp"],
)
"""Use Cabal to build a library.

Example:
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

"""

def _haskell_cabal_binary_impl(ctx):
    hs = haskell_context(ctx)
    dep_info = gather_dep_info(ctx, ctx.attr.deps)
    cc = cc_interop_info(ctx)
    cc_info = cc_common.merge_cc_infos(
        cc_infos = [dep[CcInfo] for dep in ctx.attr.deps if CcInfo in dep],
    )

    cabal = _find_cabal(hs, ctx.files.srcs)
    setup = _find_setup(hs, cabal, ctx.files.srcs)
    package_database = hs.actions.declare_file(
        "_install/package.conf.d/package.cache",
        sibling = cabal,
    )
    binary = hs.actions.declare_file(
        "_install/bin/{}".format(hs.label.name),
        sibling = cabal,
    )
    data_dir = hs.actions.declare_directory(
        "_install/data",
        sibling = cabal,
    )
    (tool_inputs, tool_input_manifests) = ctx.resolve_tools(tools = ctx.attr.tools)
    c = _prepare_cabal_inputs(
        hs,
        cc,
        dep_info,
        cc_info,
        tool_inputs = tool_inputs,
        tool_input_manifests = tool_input_manifests,
        cabal = cabal,
        setup = setup,
        srcs = ctx.files.srcs,
        cabal_wrapper_tpl = ctx.file._cabal_wrapper_tpl,
        package_database = package_database,
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
        env = c.env,
        mnemonic = "HaskellCabalBinary",
        progress_message = "HaskellCabalBinary {}".format(hs.label),
        use_default_shell_env = True,
    )

    hs_info = HaskellInfo(
        package_databases = dep_info.package_databases,
        version_macros = set.empty(),
        source_files = set.empty(),
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
            collect_default = True,
        ),
    )

    return [hs_info, cc_info, default_info]

haskell_cabal_binary = rule(
    _haskell_cabal_binary_impl,
    executable = True,
    attrs = {
        "srcs": attr.label_list(allow_files = True),
        "deps": attr.label_list(),
        "tools": attr.label_list(
            cfg = "host",
            doc = """Tool dependencies. They are built using the host configuration, since
            the tools are executed as part of the build.""",
        ),
        "_cabal_wrapper_tpl": attr.label(
            allow_single_file = True,
            default = Label("@io_tweag_rules_haskell//haskell:private/cabal_wrapper.sh.tpl"),
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
    fragments = ["cpp"],
)
"""Use Cabal to build a binary.

Example:
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

"""

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
    "xhtml",
]

def _compute_dependency_graph(repository_ctx, versioned_packages, unversioned_packages):
    """Given a list of root packages, compute a dependency graph.

    Returns:
      dependencies: adjacency list of packages, represented as a dictionary.
      transitive_unpacked_sdists: directory names of unpacked source distributions.

    """

    if not versioned_packages and not unversioned_packages:
        return ({}, [])

    stack_cmd = repository_ctx.which("stack")
    if not stack_cmd:
        fail("Cannot find stack command in your PATH.")
    exec_result = _execute_or_fail_loudly(repository_ctx, [stack_cmd, "--version"])
    stack_major_version = int(exec_result.stdout.split(" ")[0].split(".")[0])
    if stack_major_version < 2:
        fail("Stack 2.1 or above required.")

    # Unpack all given packages, then compute the transitive closure
    # and unpack anything in the transitive closure as well.
    stack = [stack_cmd]
    if versioned_packages:
        _execute_or_fail_loudly(repository_ctx, stack + ["unpack"] + versioned_packages)
    stack = [stack_cmd, "--resolver", repository_ctx.attr.snapshot]
    if unversioned_packages:
        _execute_or_fail_loudly(repository_ctx, stack + ["unpack"] + unversioned_packages)
    exec_result = _execute_or_fail_loudly(repository_ctx, ["ls"])
    unpacked_sdists = exec_result.stdout.splitlines()
    stack_yaml_content = struct(resolver = "none", packages = unpacked_sdists).to_json()
    repository_ctx.file("stack.yaml", content = stack_yaml_content, executable = False)
    exec_result = _execute_or_fail_loudly(
        repository_ctx,
        stack + ["ls", "dependencies", "--global-hints", "--separator=-"],
    )
    transitive_unpacked_sdists = [
        unpacked_sdist
        for unpacked_sdist in exec_result.stdout.splitlines()
        if _chop_version(unpacked_sdist) not in _CORE_PACKAGES
        if _chop_version(unpacked_sdist) not in _CABAL_TOOLS
    ]
    for unpacked_sdist in transitive_unpacked_sdists:
        package = _chop_version(unpacked_sdist)
        if _version(unpacked_sdist) == "<unknown>":
            fail("""\
Could not resolve version of {}. It is not in the snapshot.
Specify a fully qualified package name of the form <package>-<version>.
            """.format(package))
    _execute_or_fail_loudly(
        repository_ctx,
        stack + ["unpack"] + [
            unpacked_sdist
            for unpacked_sdist in transitive_unpacked_sdists
            if unpacked_sdist not in unpacked_sdists
        ],
    )
    stack_yaml_content = struct(resolver = "none", packages = transitive_unpacked_sdists).to_json()
    repository_ctx.file("stack.yaml", stack_yaml_content, executable = False)

    # Compute dependency graph.
    all_packages = [_chop_version(dir) for dir in transitive_unpacked_sdists + _CORE_PACKAGES]
    exec_result = _execute_or_fail_loudly(
        repository_ctx,
        stack + ["dot", "--global-hints", "--external"],
    )
    dependencies = {k: [] for k in all_packages}
    for line in exec_result.stdout.splitlines():
        tokens = [w.strip('";') for w in line.split(" ")]

        # All lines of the form `"foo" -> "bar";` declare edges of the
        # dependency graph in the Graphviz format.
        if len(tokens) == 3 and tokens[1] == "->":
            [src, _, dest] = tokens
            if src in all_packages and dest in all_packages:
                dependencies[src].append(dest)
    return (dependencies, transitive_unpacked_sdists)

def _stack_snapshot_impl(repository_ctx):
    packages = repository_ctx.attr.packages
    non_core_packages = [
        package
        for package in packages
        if package not in _CORE_PACKAGES
    ]
    versioned_packages = []
    unversioned_packages = []
    for package in non_core_packages:
        if _has_version(package):
            versioned_packages.append(package)
        else:
            unversioned_packages.append(package)
    (dependencies, transitive_unpacked_sdists) = _compute_dependency_graph(
        repository_ctx,
        versioned_packages,
        unversioned_packages,
    )

    # Write out the dependency graph as a BUILD file.
    build_file_builder = []
    build_file_builder.append("""
load("@io_tweag_rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_toolchain_library")
""")
    extra_deps = [
        "@{}//{}:{}".format(label.workspace_name, label.package, label.name)
        for label in repository_ctx.attr.deps
    ]
    tools = [
        "@{}//{}:{}".format(label.workspace_name, label.package, label.name)
        for label in repository_ctx.attr.tools
    ]
    for package in _CORE_PACKAGES:
        if package in packages:
            visibility = ["//visibility:public"]
        else:
            visibility = ["//visibility:private"]
        build_file_builder.append(
            """
haskell_toolchain_library(name = "{name}", visibility = {visibility})
""".format(name = package, visibility = visibility),
        )
    for package in transitive_unpacked_sdists:
        unversioned_package = _chop_version(package)
        if unversioned_package in _CORE_PACKAGES:
            continue
        if unversioned_package in unversioned_packages or package in versioned_packages:
            visibility = ["//visibility:public"]
        else:
            visibility = ["//visibility:private"]
        build_file_builder.append(
            """
haskell_cabal_library(
    name = "{name}",
    srcs = glob(["{dir}/**"]),
    deps = {deps},
    tools = {tools},
    visibility = {visibility},
)
""".format(
                name = package,
                dir = package,
                deps = dependencies[unversioned_package] + extra_deps,
                tools = tools,
                testonly = repository_ctx.attr.testonly,
                visibility = visibility,
            ),
        )
        build_file_builder.append(
            """alias(name = "{name}", actual = ":{actual}", visibility = {visibility})""".format(
                name = unversioned_package,
                actual = package,
                visibility = visibility,
            ),
        )
    build_file_content = "\n".join(build_file_builder)
    repository_ctx.file("BUILD.bazel", build_file_content, executable = False)

stack_snapshot = repository_rule(
    _stack_snapshot_impl,
    attrs = {
        "snapshot": attr.string(
            doc = "The name of a Stackage snapshot.",
        ),
        "packages": attr.string_list(
            doc = "A set of package identifiers. For packages in the snapshot, version numbers can be omitted.",
        ),
        "deps": attr.label_list(
            doc = "Dependencies of the package set, e.g. system libraries or C/C++ libraries.",
        ),
        "tools": attr.label_list(
            doc = """Tool dependencies. They are built using the host configuration, since
            the tools are executed as part of the build.""",
        ),
    },
)
"""Use Stack to download and extract Cabal source distributions.

Example:
  ```bzl
  stack_snapshot(
      name = "stackage",
      packages = ["conduit", "lens", "zlib-0.6.2"],
      tools = ["@happy//:happy", "@c2hs//:c2hs"],
      snapshot = "lts-13.15",
      deps = ["@zlib.dev//:zlib"],
  )
  ```
  defines `@stackage//:conduit`, `@stackage//:lens`,
  `@stackage//:zlib` library targets.

This rule will use Stack to compute the transitive closure of the
subset of the given snapshot listed in the `packages` attribute, and
generate a dependency graph. If a package in the closure depends on
system libraries or other external libraries, use the `deps` attribute
to list them. This attribute works like the
`--extra-{include,lib}-dirs` flags for Stack and cabal-install do.

Packages that are in the snapshot need not have their versions
specified. But any additional packages or version overrides will have
to be specified with a package identifier of the form
`<package>-<version>` in the `packages` attribute.

In the external repository defined by the rule, all given packages are
available as top-level targets named after each package.

"""
