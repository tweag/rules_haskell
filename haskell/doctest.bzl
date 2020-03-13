"""Doctest support"""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load(":cc.bzl", "cc_interop_info", "ghc_cc_program_args")
load(":private/context.bzl", "haskell_context", "render_env")
load(":private/set.bzl", "set")
load(
    "@rules_haskell//haskell:providers.bzl",
    "HaskellCcLibrariesInfo",
    "HaskellInfo",
)
load(
    "@rules_haskell//haskell:private/cc_libraries.bzl",
    "get_ghci_library_files",
    "haskell_cc_libraries_aspect",
    "link_libraries",
)

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
            allow_single_file = True,
            mandatory = True,
        ),
    },
)

def haskell_doctest_toolchain(name, doctest, **kwargs):
    """Declare a toolchain for the `haskell_doctest` rule.

    You need at least one of these declared somewhere in your `BUILD`files
    for `haskell_doctest` to work.  Once declared, you then need to *register*
    the toolchain using `register_toolchains` in your `WORKSPACE` file.

    ### Examples

      In a `BUILD` file:

      ```bzl
      haskell_doctest_toolchain(
        name = "doctest",
        doctest = "@doctest//:bin",
      )
      ```
      And in `WORKSPACE`:
      ```
      register_toolchains("//:doctest")
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
        toolchain_type = "@rules_haskell//haskell:doctest-toolchain",
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

    if HaskellInfo not in target:
        return []

    hs = haskell_context(ctx, ctx.attr)
    posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]

    hs_info = target[HaskellInfo]
    cc_info = target[CcInfo]
    cc_libraries_info = target[HaskellCcLibrariesInfo]

    args = ctx.actions.args()
    args.add("--no-magic")

    cc = cc_interop_info(ctx)
    args.add_all(ghc_cc_program_args(cc.tools.cc))

    doctest_log = ctx.actions.declare_file(
        "doctest-log-" + ctx.label.name + "-" + target.label.name,
    )

    toolchain = ctx.toolchains["@rules_haskell//haskell:doctest-toolchain"]

    # GHC flags we have prepared before.
    args.add_all(hs_info.compile_flags)

    # Add any extra flags specified by the user.
    args.add_all(ctx.attr.doctest_flags)

    # C library dependencies to link against.
    link_libraries(
        get_ghci_library_files(hs, cc_libraries_info, cc.cc_libraries),
        args,
        prefix_optl = hs.toolchain.is_darwin,
    )

    if ctx.attr.modules:
        inputs = ctx.attr.modules
        args.add_all(set.to_list(hs_info.import_dirs), format_each = "-i%s")
    else:
        inputs = [source.path for source in hs_info.source_files.to_list()]

    ctx.actions.run_shell(
        inputs = depset(transitive = [
            hs_info.source_files,
            hs_info.package_databases,
            hs_info.interface_dirs,
            hs_info.extra_source_files,
            hs_info.dynamic_libraries,
            cc_info.compilation_context.headers,
            depset(get_ghci_library_files(hs, cc_libraries_info, cc.transitive_libraries)),
            depset(
                toolchain.doctest +
                cc.files +
                [hs.tools.ghc],
            ),
        ]),
        outputs = [doctest_log],
        mnemonic = "HaskellDoctest",
        progress_message = "HaskellDoctest {}".format(ctx.label),
        command = """
        {env}
        {doctest} "$@" {inputs} > {output} 2>&1 || (rc=$? && cat {output} && exit $rc)
        """.format(
            doctest = toolchain.doctest[0].path,
            output = doctest_log.path,
            inputs = " ".join(inputs),
            # XXX Workaround
            # https://github.com/bazelbuild/bazel/issues/5980.
            env = render_env(hs.env),
        ),
        arguments = [args],
        env = hs.env,
        execution_requirements = {
            # Prevents a race condition among concurrent doctest tests on Linux.
            #
            # The reason is that the doctest process uses its own PID to determine the name
            # of its working directory. In presence of PID namespacing, this occasionally results
            # in multiple concurrent processes attempting to create the same directory.
            # See https://github.com/sol/doctest/issues/219 for details.
            #
            # For some reason, setting "exclusive": "1" does not fix the issue, so we disable
            # sandboxing altogether for doctest tests.
            "no-sandbox": "1",
        },
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
            aspects = [haskell_cc_libraries_aspect],
        ),
        "doctest_flags": attr.string_list(
            doc = "Extra flags to pass to doctest executable.",
        ),
        "modules": attr.string_list(
            doc = """List of names of modules that will be tested. If the list is
omitted, all exposed modules provided by `deps` will be tested.
""",
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
    fragments = ["cpp"],
    toolchains = [
        "@bazel_tools//tools/cpp:toolchain_type",
        "@rules_haskell//haskell:toolchain",
        "@rules_haskell//haskell:doctest-toolchain",
        "@rules_sh//sh/posix:toolchain_type",
    ],
)
"""Run doctest test on targets in `deps`.

Note that your toolchain must be equipped with `doctest` executable, i.e.
you should specify location of the executable using the `doctest` attribute
of `haskell_doctest_toolchain`.
"""
