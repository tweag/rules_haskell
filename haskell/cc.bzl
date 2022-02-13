"""Interop with cc_* rules

These rules are deprecated.
"""

load(
    "@rules_cc//cc:action_names.bzl",
    "CPP_LINK_EXECUTABLE_ACTION_NAME",
    "C_COMPILE_ACTION_NAME",
)
load("@rules_cc//cc:find_cc_toolchain.bzl", "find_cc_toolchain")
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
        "env": "Dictionary with environment variables to call the CC toolchain",
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
        "cc_th_libraries": "depset, C libraries from TemplateHaskell dependencies.",
        "transitive_th_libraries": "depset, C and Haskell libraries from transitive TemplateHaskell dependencies.",
        "plugin_libraries": "depset, C and Haskell libraries from transitive plugin dependencies.",
        "setup_libraries": "depset, C and Haskell libraries from Cabal setup dependencies.",
    },
)

def cc_interop_info(ctx, override_cc_toolchain = None):
    """Gather information from any CC dependencies.

    *Internal function - do not use.*

    Args:
      ctx: Rule context.

    Returns:
      CcInteropInfo: Information needed for CC interop.
    """
    ccs = [dep[CcInfo] for dep in ctx.attr.deps if CcInfo in dep and HaskellInfo not in dep]

    if hasattr(ctx.attr, "th_deps"):
        th_deps = ctx.attr.th_deps
        if not th_deps and ctx.attr.use_deps_on_empty_th_deps:
            th_deps = ctx.attr.deps
    else:
        th_deps = ctx.attr.deps

    th_ccs = [dep[CcInfo] for dep in th_deps if CcInfo in dep and HaskellInfo not in dep]

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

    # XXX: protobuf is passing a "patched ctx"
    # which includes the real ctx as "real_ctx"
    real_ctx = getattr(ctx, "real_ctx", ctx)

    # Asterius does not behave as other ghc cross compilers yet and
    # relies on a cc toolchain targeting the exec platform .
    cc_toolchain = override_cc_toolchain or find_cc_toolchain(real_ctx)

    feature_configuration = cc_common.configure_features(
        ctx = real_ctx,
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
        # See https://github.com/bazelbuild/bazel/issues/6876
        action_name = CPP_LINK_EXECUTABLE_ACTION_NAME,
        variables = link_variables,
    )
    real_cc_path = cc_common.get_tool_for_action(
        feature_configuration = feature_configuration,
        action_name = C_COMPILE_ACTION_NAME,
    )

    # Generate cc wrapper script on Darwin that adjusts load commands.
    hs_toolchain = ctx.toolchains["@rules_haskell//haskell:toolchain"]
    cc_wrapper = hs_toolchain.cc_wrapper
    cc = cc_wrapper.executable.path
    cc_files = depset(transitive = [cc_toolchain.all_files, cc_wrapper.inputs])
    cc_manifests = cc_wrapper.manifests

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

    env = {}
    if hs_toolchain.is_darwin:
        env["CC_WRAPPER_PLATFORM"] = "darwin"
    elif hs_toolchain.is_windows:
        env["CC_WRAPPER_PLATFORM"] = "windows"
    else:
        env["CC_WRAPPER_PLATFORM"] = "linux"

    env["CC_WRAPPER_CC_PATH"] = real_cc_path
    env["CC_WRAPPER_CPU"] = cc_toolchain.cpu

    cc_libraries_info = deps_HaskellCcLibrariesInfo(
        ctx.attr.deps + th_deps + getattr(ctx.attr, "plugins", []) + getattr(ctx.attr, "setup_deps", []),
    )
    return CcInteropInfo(
        tools = struct(**tools),
        env = env,
        files = cc_files.to_list(),
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
        cc_libraries = get_cc_libraries(cc_libraries_info, [lib for li in cc_common.merge_cc_infos(cc_infos = ccs).linking_context.linker_inputs.to_list() for lib in li.libraries]),
        cc_th_libraries = get_cc_libraries(cc_libraries_info, [
            lib
            for li in cc_common.merge_cc_infos(cc_infos = th_ccs).linking_context.linker_inputs.to_list()
            for lib in li.libraries
        ]),
        transitive_libraries = [lib for li in cc_common.merge_cc_infos(cc_infos = [
            dep[CcInfo]
            for dep in ctx.attr.deps
            if CcInfo in dep
        ]).linking_context.linker_inputs.to_list() for lib in li.libraries],
        transitive_th_libraries = [lib for li in cc_common.merge_cc_infos(cc_infos = [
            dep[CcInfo]
            for dep in th_deps
            if CcInfo in dep
        ]).linking_context.linker_inputs.to_list() for lib in li.libraries],
        plugin_libraries = [lib for li in cc_common.merge_cc_infos(cc_infos = [
            dep[CcInfo]
            for plugin in getattr(ctx.attr, "plugins", [])
            for dep in plugin[GhcPluginInfo].deps
            if CcInfo in dep
        ]).linking_context.linker_inputs.to_list() for lib in li.libraries],
        setup_libraries = [lib for li in cc_common.merge_cc_infos(cc_infos = [
            dep[CcInfo]
            for dep in getattr(ctx.attr, "setup_deps", [])
            if CcInfo in dep
        ]).linking_context.linker_inputs.to_list() for lib in li.libraries],
    )

def ghc_cc_program_args(hs, cc):
    """Returns the -pgm* flags required to override cc.

    Args:
      cc: string, path to the C compiler (cc_wrapper).

    Returns:
      list of string, GHC arguments.
    """
    args = [
        # GHC uses C compiler for assemly, linking and preprocessing as well.
        "-pgma",
        cc,
        "-pgmc",
        cc,
        "-pgml",
        cc,
        # Setting -pgm* flags explicitly has the unfortunate side effect
        # of resetting any program flags in the GHC settings file. So we
        # restore them here. See
        # https://ghc.haskell.org/trac/ghc/ticket/7929.
        #
        # Since GHC 8.8 the semantics of `-optP` have changed and these flags
        # are now also forwarded to `cc` via `-Xpreprocessor`, which breaks the
        # default flags `-E -undef -traditional`. GHC happens to word split the
        # argument to `-pgmP` which allows to pass these flags to `gcc` itself
        # as the preprocessor. See
        # https://gitlab.haskell.org/ghc/ghc/issues/17185#note_261599.
        "-pgmP",
        cc + " -E -undef -traditional",
        "-optc-fno-stack-protector",
    ]
    if hs.toolchain.numeric_version >= [8, 10, 3]:
        args.append("-pgmc-supports-no-pie")
    return args
