"""Doctest support"""

load(
    ":private/providers.bzl",
    "HaskellBinaryInfo",
    "HaskellBuildInfo",
    "HaskellLibraryInfo",
)
load(":private/context.bzl", "haskell_context")
load(":private/set.bzl", "set")
load(
    ":private/path_utils.bzl",
    "get_external_libs_path",
    "get_lib_name",
    "target_unique_name",
)
load("@bazel_skylib//:lib.bzl", "dicts", "paths")

def _doctest_toolchain_impl(ctx):
    return platform_common.ToolchainInfo(
        name = ctx.label.name,
        doctest = ctx.files.doctest,
    )

_doctest_toolchain = rule(
    _doctest_toolchain_impl,
    attrs = {
        "doctest": attr.label(
            doc = "Doctest executable",
            cfg = "host",
            executable = True,
            single_file = True,
            mandatory = True,
        ),
    },
)

def haskell_doctest_toolchain(name, doctest, **kwargs):
    """Declare a toolchain for the `haskell_doctest` rule.

    You need at least one of these declared somewhere in your `BUILD`files
    for `haskell_doctest` to work.  Once declared, you then need to *register*
    the toolchain using `register_toolchain` in your `WORKSPACE` file.

    Example:

      In a `BUILD` file:

      ```bzl
      haskell_doctest_toolchain(
        name = "doctest",
        doctest = "@doctest//:bin",
      )
      ```
      And in `WORKSPACE`:
      ```
      register_toolchain("//:doctest")
      ```
    """
    impl_name = name + "-impl"
    _doctest_toolchain(
        name = impl_name,
        doctest = doctest,
        visibility = ["//visibility:public"],
        **kwargs
    )
    native.toolchain(
        name = name,
        toolchain_type = "@io_tweag_rules_haskell//haskell:doctest-toolchain",
        toolchain = ":" + impl_name,
    )

def _haskell_doctest_single(target, ctx):
    """Doctest a single Haskell `target`.

    Args:
      target: Provider(s) of the target to doctest.
      ctx: Rule context.

    Returns:
      File: the doctest log.
    """

    if HaskellBuildInfo not in target:
        return []

    hs = haskell_context(ctx, ctx.attr)

    build_info = target[HaskellBuildInfo]
    lib_info = target[HaskellLibraryInfo] if HaskellLibraryInfo in target else None
    bin_info = target[HaskellBinaryInfo] if HaskellBinaryInfo in target else None

    args = ctx.actions.args()
    args.add(["--no-magic"])

    doctest_log = ctx.actions.declare_file(
        "doctest-log-" + ctx.label.name + "-" + (
            lib_info.package_id if lib_info != None else bin_info.binary.basename
        ),
    )

    toolchain = ctx.toolchains["@io_tweag_rules_haskell//haskell:doctest-toolchain"]

    # GHC flags we have prepared before.
    args.add(lib_info.ghc_args if lib_info != None else bin_info.ghc_args)

    # Add any extra flags specified by the user.
    args.add(ctx.attr.doctest_flags)

    # External libraries.
    external_libs = set.from_list(build_info.external_libraries.values())
    seen_libs = set.empty()
    for lib in set.to_list(external_libs):
        lib_name = get_lib_name(lib)
        if not set.is_member(seen_libs, lib_name):
            set.mutable_insert(seen_libs, lib_name)
            if hs.toolchain.is_darwin:
                args.add([
                    "-optl-l{0}".format(lib_name),
                    "-optl-L{0}".format(paths.dirname(lib.path)),
                ])
            else:
                args.add([
                    "-l{0}".format(lib_name),
                    "-L{0}".format(paths.dirname(lib.path)),
                ])

    header_files = lib_info.header_files if lib_info != None else bin_info.header_files

    sources = set.to_list(
        lib_info.source_files if lib_info != None else bin_info.source_files,
    )

    all_modules = set.to_list(
        lib_info.exposed_modules if lib_info != None else bin_info.modules,
    )

    selected_modules = []

    if ctx.attr.modules:
        for m in all_modules:
            if m in ctx.attr.modules:
                selected_modules.append(m)
    else:
        selected_modules = all_modules

    args.add(selected_modules)

    ctx.actions.run_shell(
        inputs = depset(transitive = [
            depset(sources),
            set.to_depset(build_info.package_confs),
            set.to_depset(build_info.package_caches),
            set.to_depset(build_info.interface_files),
            set.to_depset(build_info.dynamic_libraries),
            set.to_depset(header_files),
            set.to_depset(external_libs),
            depset(
                toolchain.doctest +
                [hs.tools.cat, hs.tools.ghc],
            ),
        ]),
        outputs = [doctest_log],
        mnemonic = "HaskellDoctest",
        progress_message = "HaskellDoctest {}".format(ctx.label),
        command = """
    {doctest} "$@" > {output} 2>&1 || rc=$? && cat {output} && exit $rc
    """.format(doctest = toolchain.doctest[0].path, output = doctest_log.path),
        arguments = [args],
        # NOTE It looks like we must specify the paths here as well as via -L
        # flags because there are at least two different "consumers" of the info
        # (ghc and linker?) and they seem to prefer to get it in different ways
        # in this case.
        env = dicts.add(
            {
                "LD_LIBRARY_PATH": get_external_libs_path(external_libs),
            },
            hs.env,
        ),
    )
    return doctest_log

def _haskell_doctest_impl(ctx):
    logs = []

    for dep in ctx.attr.deps:
        logs.append(_haskell_doctest_single(dep, ctx))

    return DefaultInfo(
        files = depset(logs),
    )

haskell_doctest = rule(
    _haskell_doctest_impl,
    attrs = {
        "deps": attr.label_list(
            doc = "List of Haskell targets to lint.",
        ),
        "doctest_flags": attr.string_list(
            doc = "Extra flags to pass to doctest executable.",
        ),
        "modules": attr.string_list(
            doc = """List of names of modules that will be tested. If the list is
omitted, all exposed modules provided by `deps` will be tested.
""",
        ),
    },
    toolchains = [
        "@io_tweag_rules_haskell//haskell:toolchain",
        "@io_tweag_rules_haskell//haskell:doctest-toolchain",
    ],
)
"""Run doctest test on targets in `deps`.

Note that your toolchain must be equipped with `doctest` executable, i.e.
you should specify location of the executable using the `doctest` attribute
of `haskell_doctest_toolchain`.
"""
