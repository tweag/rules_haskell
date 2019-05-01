"""Core Haskell rules"""

load(
    ":toolchain.bzl",
    _haskell_toolchain = "haskell_toolchain",
)

def _haskell_toolchain_library_impl(ctx):
    toolchain = ctx.toolchains["@io_tweag_rules_haskell//haskell:toolchain"]
    package = ctx.label.name
    target = toolchain.libraries.get(package)

    return [
        target[CcInfo],
    ]

def _haskell_import_impl(ctx):
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
        dynamic_library = ctx.file.shared_library,
        static_library = ctx.file.static_library,
        cc_toolchain = cc_toolchain,
    )
    compilation_context = cc_common.create_compilation_context()
    linking_context = cc_common.create_linking_context(libraries_to_link = [library_to_link])
    cc_info = CcInfo(
        compilation_context = compilation_context,
        linking_context = linking_context,
    )

    return [
        cc_info,
    ]

haskell_import = rule(
    _haskell_import_impl,
    attrs = {
        "static_library": attr.label(allow_single_file = [".a"]),
        "shared_library": attr.label(allow_single_file = [".dll", ".dylib", ".so"]),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
)

haskell_toolchain_library = rule(
    _haskell_toolchain_library_impl,
    toolchains = [
        "@io_tweag_rules_haskell//haskell:toolchain",
    ],
)

haskell_toolchain = _haskell_toolchain
