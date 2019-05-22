"""Cabal packages"""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")
load(":cc.bzl", "cc_interop_info")
load(":private/context.bzl", "haskell_context")
load(":private/dependencies.bzl", "gather_dep_info")
load(":private/mode.bzl", "is_profiling_enabled")
load(":private/set.bzl", "set")
load(
    ":providers.bzl",
    "HaskellInfo",
    "HaskellLibraryInfo",
    "empty_HaskellCcInfo",
    "get_libs_for_ghc_linker",
    "merge_HaskellCcInfo",
)

def _execute_or_fail_loudly(repository_ctx, arguments):
    exec_result = repository_ctx.execute(arguments)
    if exec_result.return_code != 0:
        fail("\n".join(["Command failed: " + " ".join(arguments), exec_result.stderr]))
    return exec_result

def _so_extension(hs):
    return "dylib" if hs.toolchain.is_darwin else "so"

def _dirname(file):
    return file.dirname

def _haskell_cabal_library_impl(ctx):
    hs = haskell_context(ctx)
    dep_info = gather_dep_info(ctx, ctx.attr.deps)
    cc = cc_interop_info(ctx)
    cc_info = cc_common.merge_cc_infos(
        cc_infos = [dep[CcInfo] for dep in ctx.attr.deps if CcInfo in dep],
    )
    name = hs.label.name
    with_profiling = is_profiling_enabled(hs)

    # Check that a .cabal file exists. Choose the root one.
    cabal = None
    for f in ctx.files.srcs:
        if f.extension == "cabal":
            if not cabal or f.dirname < cabal.dirname:
                cabal = f
    if not cabal:
        fail("A .cabal file was not found in the srcs attribute.")

    # Check that a Setup script exists. If not, create a default one.
    setup = None
    for f in ctx.files.srcs:
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

    package_database = hs.actions.declare_file(
        "_install/package.conf.d/package.cache",
        sibling = cabal,
    )
    interfaces_dir = hs.actions.declare_directory(
        "_install/iface",
        sibling = cabal,
    )
    static_library = hs.actions.declare_file(
        "_install/lib/libHS{}.a".format(name),
        sibling = cabal,
    )
    static_library_prof = None
    if with_profiling:
        static_library_prof = hs.actions.declare_file(
            "_install/lib/libHS{}_p.a".format(name),
            sibling = cabal,
        )
    dynamic_library = hs.actions.declare_file(
        "_install/lib/libHS{}-ghc{}.{}".format(name, hs.toolchain.version, _so_extension(hs)),
        sibling = cabal,
    )

    args = hs.actions.args()
    package_databases = set.to_depset(dep_info.package_databases)
    extra_headers = cc_info.compilation_context.headers
    extra_include_dirs = cc_info.compilation_context.includes
    (library_deps, ld_library_deps, ghc_env) = get_libs_for_ghc_linker(
        hs,
        dep_info.transitive_cc_dependencies,
    )
    extra_lib_dirs = [file.dirname for file in library_deps]
    args.add_all([name, setup, cabal.dirname, package_database.dirname])
    args.add_all(package_databases, map_each = _dirname, format_each = "--package-db=%s")
    args.add_all(extra_include_dirs, format_each = "--extra-include-dirs=%s")
    args.add_all(extra_lib_dirs, format_each = "--extra-lib-dirs=%s", uniquify = True)
    if with_profiling:
        args.add("--enable-profiling")

    # TODO Instantiating this template could be done just once in the
    # toolchain rule.
    cabal_wrapper = ctx.actions.declare_file("cabal_wrapper-{}.sh".format(hs.label.name))
    ctx.actions.expand_template(
        template = ctx.file._cabal_wrapper_tpl,
        output = cabal_wrapper,
        is_executable = True,
        substitutions = {
            "%{ghc}": hs.tools.ghc.path,
            "%{ghc_pkg}": hs.tools.ghc_pkg.path,
            "%{runghc}": hs.tools.runghc.path,
            "%{ar}": cc.tools.ar,
            "%{strip}": cc.tools.strip,
        },
    )

    # Make the Cabal configure/build/install steps one big action so
    # that we don't have to track all inputs explicitly between steps.
    ctx.actions.run(
        executable = cabal_wrapper,
        arguments = [args],
        inputs = depset(
            [setup, hs.tools.ghc, hs.tools.ghc_pkg, hs.tools.runghc],
            transitive = [
                depset(ctx.files.srcs),
                package_databases,
                extra_headers,
                depset(library_deps),
                depset(ld_library_deps),
                set.to_depset(dep_info.interface_dirs),
                depset(dep_info.static_libraries),
                set.to_depset(dep_info.dynamic_libraries),
            ],
        ),
        outputs = [
            package_database,
            interfaces_dir,
            static_library,
            dynamic_library,
        ] + ([static_library_prof] if with_profiling else []),
        env = dicts.add(ghc_env, hs.env),
        mnemonic = "HaskellCabalLibrary",
        progress_message = "HaskellCabalLibrary {}".format(hs.label),
        use_default_shell_env = True,
    )

    default_info = DefaultInfo(files = depset([static_library, dynamic_library]))
    hs_info = HaskellInfo(
        package_ids = set.empty(),
        package_databases = set.insert(dep_info.package_databases, package_database),
        version_macros = set.empty(),
        source_files = set.empty(),
        extra_source_files = depset(),
        import_dirs = set.empty(),
        static_libraries = [static_library] + dep_info.static_libraries,
        static_libraries_prof = (
            [static_library_prof] if with_profiling else []
        ) + dep_info.static_libraries_prof,
        dynamic_libraries = set.insert(dep_info.dynamic_libraries, dynamic_library),
        interface_dirs = set.insert(dep_info.interface_dirs, interfaces_dir),
        compile_flags = [],
        prebuilt_dependencies = set.empty(),
        direct_prebuilt_deps = set.empty(),
        cc_dependencies = dep_info.cc_dependencies,
        transitive_cc_dependencies = dep_info.transitive_cc_dependencies,
    )
    lib_info = HaskellLibraryInfo(package_id = name, version = None)
    cc_toolchain = find_cpp_toolchain(ctx)
    feature_configuration = cc_common.configure_features(
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
        "_cabal_wrapper_tpl": attr.label(
            allow_single_file = True,
            default = Label("@io_tweag_rules_haskell//haskell:private/cabal_wrapper.sh.tpl"),
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
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
