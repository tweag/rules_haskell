"""Core Haskell rules"""

load(
    ":toolchain.bzl",
    _haskell_toolchain = "haskell_toolchain",
)

load(":private/context.bzl", "haskell_context")

def _haskell_toolchain_library_impl(ctx):
    hs = haskell_context(ctx)

    if ctx.attr.package:
        package = ctx.attr.package
    else:
        package = ctx.label.name

    target = hs.toolchain.libraries.get(package)
    if not target:
        fail("""
{} is not a toolchain library.
Check that it ships with your version of GHC.
            """.format(package)
        )

    return [
        target[CcInfo],
        target[DefaultInfo],
    ]

def _haskell_import_impl(ctx):
    id = ctx.attr.id or ctx.attr.name
    target_files = [
        file for file in [ctx.file.static_library, ctx.file.shared_library] if file
    ]
    # XXX Workaround https://github.com/bazelbuild/bazel/issues/6874.
    # Should be find_cpp_toolchain() instead.
    cc_toolchain = ctx.attr._cc_toolchain[cc_common.CcToolchainInfo]
    feature_configuration = cc_common.configure_features(
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )
    library_to_link = cc_common.create_library_to_link(
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        # TODO Reenable dynamic linking for Haskell libraries. See
        # https://github.com/bazelbuild/bazel/issues/8129.
        #
        dynamic_library = ctx.file.shared_library,
        static_library = ctx.file.static_library,
        cc_toolchain = cc_toolchain,
    )
    compilation_context = cc_common.create_compilation_context(
    )
    linking_context = cc_common.create_linking_context(
        libraries_to_link = [library_to_link],
    )
    cc_info = cc_common.merge_cc_infos(
        cc_infos = [
            CcInfo(
                compilation_context = compilation_context,
                linking_context = linking_context,
            )
        ] + [dep[CcInfo] for dep in ctx.attr.deps if CcInfo in dep]
    )

    return [
        cc_info,
    ]

haskell_import = rule(
    _haskell_import_impl,
    attrs = {
        "id": attr.string(),
        "version": attr.string(),
        "deps": attr.label_list(),
        "static_library": attr.label(allow_single_file = [".a"]),
        "shared_library": attr.label(allow_single_file = [".dll", ".dylib", ".so"]),
        "static_profiling_library": attr.label(allow_single_file = ["_p.a"]),
        "linkopts": attr.string_list(),
        "hdrs": attr.label_list(allow_files = True),
        "includes": attr.string_list(),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
)

haskell_toolchain_library = rule(
    _haskell_toolchain_library_impl,
    attrs = dict(
        package = attr.string(
            doc = "The name of a GHC package not built by Bazel. Defaults to the name of the rule.",
        ),
        _version_macros = attr.label(
            executable = True,
            cfg = "host",
            default = Label("@io_tweag_rules_haskell//haskell:version_macros"),
        ),
    ),
    toolchains = [
        "@io_tweag_rules_haskell//haskell:toolchain",
    ],
)

haskell_toolchain = _haskell_toolchain
