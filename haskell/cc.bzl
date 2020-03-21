"""Interop with cc_* rules

These rules are deprecated.
"""

load(
    "@bazel_tools//tools/build_defs/cc:action_names.bzl",
    "CPP_LINK_DYNAMIC_LIBRARY_ACTION_NAME",
    "C_COMPILE_ACTION_NAME",
)
load(
    "//haskell:providers.bzl",
    "GhcPluginInfo",
    "HaskellInfo",
)
load(":private/cc_libraries.bzl", "deps_HaskellCcLibrariesInfo", "get_cc_libraries")

CcInteropInfo = provider(
    doc = "Information needed for interop with cc rules.",
    fields = {
        "tools": "Tools from the CC toolchain",
        # See the following for why this is needed:
        # https://stackoverflow.com/questions/52769846/custom-c-rule-with-the-cc-common-api
        "files": "Files for all tools (input to any action that uses tools)",
        "manifests": "Input manifests for all tools (input to any action that uses tools)",
        "hdrs": "CC headers",
        "cpp_flags": "Preprocessor flags",
        "compiler_flags": "Flags for compilation",
        "linker_flags": "Flags to forward to the linker",
        "include_args": "Extra include dirs",
        "cc_libraries_info": "HaskellCcLibrariesInfo",
        "cc_libraries": "depset, C libraries from direct linking dependencies.",
        "transitive_libraries": "depset, C and Haskell libraries from transitive linking dependencies.",
        "plugin_libraries": "depset, C and Haskell libraries from transitive plugin dependencies.",
    },
)

def cc_interop_info(ctx):
    """Gather information from any CC dependencies.

    *Internal function - do not use.*

    Args:
      ctx: Rule context.

    Returns:
      CcInteropInfo: Information needed for CC interop.
    """
    ccs = [dep[CcInfo] for dep in ctx.attr.deps if CcInfo in dep and HaskellInfo not in dep]

    hdrs = []
    include_args = []
    cpp_flags = []
    for cc in ccs:
        cc_ctx = cc.compilation_context
        hdrs.append(cc_ctx.headers)
        include_args.extend(["-I" + include for include in cc_ctx.includes.to_list()])
        cpp_flags.extend(
            [
                "-D" + define
                for define in cc_ctx.defines.to_list()
            ] + [
                f
                for include in cc_ctx.quote_includes.to_list()
                for f in ["-iquote", include]
            ] + [
                f
                for include in cc_ctx.system_includes.to_list()
                for f in ["-isystem", include]
            ],
        )

    hdrs = depset(transitive = hdrs)

    # XXX Workaround https://github.com/bazelbuild/bazel/issues/6874.
    # Should be find_cpp_toolchain() instead.
    cc_toolchain = ctx.attr._cc_toolchain[cc_common.CcToolchainInfo]
    feature_configuration = cc_common.configure_features(
        # XXX: protobuf is passing a "patched ctx"
        # which includes the real ctx as "real_ctx"
        ctx = getattr(ctx, "real_ctx", ctx),
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )
    compile_variables = cc_common.create_compile_variables(
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
    )
    compiler_flags = cc_common.get_memory_inefficient_command_line(
        feature_configuration = feature_configuration,
        action_name = C_COMPILE_ACTION_NAME,
        variables = compile_variables,
    )
    link_variables = cc_common.create_link_variables(
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        is_linking_dynamic_library = False,
        is_static_linking_mode = True,
    )
    linker_flags = cc_common.get_memory_inefficient_command_line(
        feature_configuration = feature_configuration,
        action_name = CPP_LINK_DYNAMIC_LIBRARY_ACTION_NAME,
        variables = link_variables,
    )

    # Generate cc wrapper script on Darwin that adjusts load commands.
    hs_toolchain = ctx.toolchains["@rules_haskell//haskell:toolchain"]
    cc_wrapper = hs_toolchain.cc_wrapper
    cc = cc_wrapper.executable.path
    cc_files = ctx.files._cc_toolchain + cc_wrapper.inputs.to_list()
    cc_manifests = cc_wrapper.manifests

    # XXX Workaround https://github.com/bazelbuild/bazel/issues/6876.
    linker_flags = [flag for flag in linker_flags if flag not in ["-shared"]]

    tools = {
        "ar": cc_toolchain.ar_executable,
        "cc": cc,
        "ld": cc_toolchain.ld_executable,
        "cpp": cc_toolchain.preprocessor_executable,
        "nm": cc_toolchain.nm_executable,
        "strip": cc_toolchain.strip_executable,
    }

    # If running on darwin but XCode is not installed (i.e., only the Command
    # Line Tools are available), then Bazel will make ar_executable point to
    # "/usr/bin/libtool". Since we call ar directly, override it.
    # TODO: remove this if Bazel fixes its behavior.
    # Upstream ticket: https://github.com/bazelbuild/bazel/issues/5127.
    if tools["ar"].find("libtool") >= 0:
        tools["ar"] = "/usr/bin/ar"

    cc_libraries_info = deps_HaskellCcLibrariesInfo(
        ctx.attr.deps + getattr(ctx.attr, "plugins", []),
    )
    return CcInteropInfo(
        tools = struct(**tools),
        files = cc_files,
        manifests = cc_manifests,
        hdrs = hdrs.to_list(),
        cpp_flags = cpp_flags,
        include_args = include_args,
        compiler_flags = compiler_flags,
        # XXX this might not be the right set of flags for all situations,
        # but this will anyways all be replaced (once implemented) by
        # https://github.com/bazelbuild/bazel/issues/4571.
        linker_flags = linker_flags,
        cc_libraries_info = cc_libraries_info,
        cc_libraries = get_cc_libraries(cc_libraries_info, cc_common.merge_cc_infos(cc_infos = ccs).linking_context.libraries_to_link.to_list()),
        transitive_libraries = cc_common.merge_cc_infos(cc_infos = [
            dep[CcInfo]
            for dep in ctx.attr.deps
            if CcInfo in dep
        ]).linking_context.libraries_to_link.to_list(),
        plugin_libraries = cc_common.merge_cc_infos(cc_infos = [
            dep[CcInfo]
            for plugin in getattr(ctx.attr, "plugins", [])
            for dep in plugin[GhcPluginInfo].deps
            if CcInfo in dep
        ]).linking_context.libraries_to_link.to_list(),
    )

def ghc_cc_program_args(cc):
    """Returns the -pgm* flags required to override cc.

    Args:
      cc: string, path to the C compiler (cc_wrapper).

    Returns:
      list of string, GHC arguments.
    """
    return [
        # GHC uses C compiler for assemly, linking and preprocessing as well.
        "-pgma",
        cc,
        "-pgmc",
        cc,
        "-pgml",
        cc,
        "-pgmP",
        cc,
        # Setting -pgm* flags explicitly has the unfortunate side effect
        # of resetting any program flags in the GHC settings file. So we
        # restore them here. See
        # https://ghc.haskell.org/trac/ghc/ticket/7929.
        "-optc-fno-stack-protector",
        "-optP-E",
        "-optP-undef",
        "-optP-traditional",
    ]
