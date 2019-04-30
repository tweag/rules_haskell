"""Interop with cc_* rules

These rules are deprecated.
"""

load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")
load(
    "@bazel_tools//tools/build_defs/cc:action_names.bzl",
    "CPP_LINK_DYNAMIC_LIBRARY_ACTION_NAME",
    "C_COMPILE_ACTION_NAME",
)
load(":private/path_utils.bzl", "ln")
load("@bazel_skylib//lib:paths.bzl", "paths")
load(":private/set.bzl", "set")
load(
    "@io_tweag_rules_haskell//haskell:providers.bzl",
    "HaskellInfo",
)

CcInteropInfo = provider(
    doc = "Information needed for interop with cc rules.",
    fields = {
        "tools": "Tools from the CC toolchain",
        # See the following for why this is needed:
        # https://stackoverflow.com/questions/52769846/custom-c-rule-with-the-cc-common-api
        "files": "Files for all tools (input to any action that uses tools)",
        "hdrs": "CC headers",
        "cpp_flags": "Preprocessor flags",
        "compiler_flags": "Flags for compilation",
        "linker_flags": "Flags to forward to the linker",
        "include_args": "Extra include dirs",
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
        include_args.extend(["-I" + include for include in cc_ctx.includes])
        cpp_flags.extend(
            [
                "-D" + define
                for define in cc_ctx.defines
            ] + [
                f
                for include in cc_ctx.quote_includes
                for f in ["-iquote", include]
            ] + [
                f
                for include in cc_ctx.system_includes
                for f in ["-isystem", include]
            ],
        )

    hdrs = depset(transitive = hdrs)

    # XXX Workaround https://github.com/bazelbuild/bazel/issues/6874.
    # Should be find_cpp_toolchain() instead.
    cc_toolchain = ctx.attr._cc_toolchain[cc_common.CcToolchainInfo]
    feature_configuration = cc_common.configure_features(
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
    hs_toolchain = ctx.toolchains["@io_tweag_rules_haskell//haskell:toolchain"]
    if hs_toolchain.is_darwin:
        cc_wrapper = ctx.actions.declare_file("osx_cc_wrapper")
        cc = cc_wrapper.path
        ctx.actions.expand_template(
            template = hs_toolchain.osx_cc_wrapper_tpl,
            output = cc_wrapper,
            substitutions = {
                "%{cc}": cc_toolchain.compiler_executable(),
            },
        )
        cc_files = ctx.files._cc_toolchain + [
            cc_wrapper,
        ]
    else:
        cc = cc_toolchain.compiler_executable()
        cc_files = ctx.files._cc_toolchain

    # XXX Workaround https://github.com/bazelbuild/bazel/issues/6876.
    linker_flags = [flag for flag in linker_flags if flag not in ["-shared"]]

    tools = {
        "ar": cc_toolchain.ar_executable(),
        "cc": cc,
        "ld": cc_toolchain.ld_executable(),
        "cpp": cc_toolchain.preprocessor_executable(),
        "nm": cc_toolchain.nm_executable(),
    }

    # If running on darwin but XCode is not installed (i.e., only the Command
    # Line Tools are available), then Bazel will make ar_executable point to
    # "/usr/bin/libtool". Since we call ar directly, override it.
    # TODO: remove this if Bazel fixes its behavior.
    # Upstream ticket: https://github.com/bazelbuild/bazel/issues/5127.
    if tools["ar"].find("libtool") >= 0:
        tools["ar"] = "/usr/bin/ar"

    return CcInteropInfo(
        tools = struct(**tools),
        files = cc_files,
        hdrs = hdrs.to_list(),
        cpp_flags = cpp_flags,
        include_args = include_args,
        compiler_flags = compiler_flags,
        # XXX this might not be the right set of flags for all situations,
        # but this will anyways all be replaced (once implemented) by
        # https://github.com/bazelbuild/bazel/issues/4571.
        linker_flags = linker_flags,
    )

def _cc_import_impl(ctx):
    strip_prefix = ctx.attr.strip_include_prefix

    # cc_library's strip_include_prefix attribute accepts both absolute and
    # relative paths.  For simplicity we currently only implement absolute
    # paths.
    if strip_prefix.startswith("/"):
        prefix = strip_prefix[1:]
    else:
        prefix = paths.join(ctx.label.workspace_root, ctx.label.package, strip_prefix)

    roots = set.empty()
    for f in ctx.files.hdrs:
        # If it's a generated file, strip off the bin or genfiles prefix.
        path = f.path
        if path.startswith(ctx.bin_dir.path):
            path = paths.relativize(path, ctx.bin_dir.path)
        elif path.startswith(ctx.genfiles_dir.path):
            path = paths.relativize(path, ctx.genfiles_dir.path)

        if not path.startswith(prefix):
            fail("Header {} does not have expected prefix {}".format(
                path,
                prefix,
            ))
        roots = set.insert(roots, f.root.path if f.root.path else ".")

    include_directories = [paths.join(root, prefix) for root in set.to_list(roots)]

    cc_toolchain = ctx.attr._cc_toolchain[cc_common.CcToolchainInfo]
    feature_configuration = cc_common.configure_features(cc_toolchain = cc_toolchain)

    compilation_context = cc_common.create_compilation_context(
        headers = depset(transitive = [l.files for l in ctx.attr.hdrs]),
        includes = depset(direct = include_directories),
    )
    linking_context = cc_common.create_linking_context(
        libraries_to_link = [
            cc_common.create_library_to_link(
                actions = ctx.actions,
                feature_configuration = feature_configuration,
                cc_toolchain = cc_toolchain,
                dynamic_library = f,
            )
            for f in ctx.attr.shared_library.files
        ],
    )

    return [
        CcInfo(
            compilation_context = compilation_context,
            linking_context = linking_context,
        ),
    ]

haskell_cc_import = rule(
    _cc_import_impl,
    attrs = {
        "shared_library": attr.label(
            # NOTE We do not list all extensions here because .so libraries may
            # have numeric suffixes like foo.so.1.2.3, and if they also have
            # SONAME with numeric suffix, matching file must be provided, so this
            # attributes must accept libraries with almost arbitrary extensions.
            # It would be easier if Skylark supported regexps.
            allow_files = True,
            doc = """A single precompiled shared library.

Bazel ensures it is available to the binary that depends on it
during runtime.
""",
        ),
        "hdrs": attr.label_list(
            allow_files = [".h"],
            doc = """

The list of header files published by this precompiled library to be
directly included by sources in dependent rules.
""",
        ),
        "strip_include_prefix": attr.string(
            doc = """
The prefix to strip from the paths of the headers of this rule.
When set, the headers in the `hdrs` attribute of this rule are
accessible at their path (relative to the repository) with this
prefix cut off.

If it's a relative path, it's taken as a package-relative one. If it's an
absolute one, it's understood as a repository-relative path.
""",
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
)
"""Imports a prebuilt shared library.

Use this to make `.so`, `.dll`, `.dylib` files residing in external
[external repositories][bazel-ext-repos] available to Haskell rules.

*This rule is temporary replacement for [cc_import][cc_import] and is
deprecated. Use [cc_library][cc_library] instead as shown in the example.*

Example:
  ```bzl
  # Deprecated, use cc_library instead.
  # haskell_cc_import(name = "zlib", shared_library = "@zlib//:lib")

  cc_library(name = "zlib", srcs = ["@zlib//:lib"])

  haskell_import(
    name = "base_pkg",
    package = "base",
  )

  haskell_binary(
    name = "crc32sum",
    srcs = ["Main.hs"],
    deps = [
      "bazel_pkg",
      ":zlib",
    ],
  )
  ```

[bazel-ext-repos]: https://docs.bazel.build/versions/master/external.html
[cc_import]: https://docs.bazel.build/versions/master/be/c-cpp.html#cc_import
[cc_library]: https://docs.bazel.build/versions/master/be/c-cpp.html#cc_library
"""

def _cc_haskell_import(ctx):
    dyn_libs = set.empty()

    if HaskellInfo in ctx.attr.dep:
        set.mutable_union(dyn_libs, ctx.attr.dep[HaskellInfo].dynamic_libraries)
    else:
        fail("{0} has to provide `HaskellInfo`".format(ctx.attr.dep.label.name))

    return [
        DefaultInfo(
            files = set.to_depset(dyn_libs),
            default_runfiles = ctx.runfiles(
                files = ctx.attr.dep.default_runfiles.files.to_list(),
                collect_default = True,
            ),
            data_runfiles = ctx.runfiles(
                files = ctx.attr.dep.data_runfiles.files.to_list(),
                collect_data = True,
            ),
        ),
    ]

cc_haskell_import = rule(
    _cc_haskell_import,
    attrs = {
        "dep": attr.label(
            doc = """
Target providing a `HaskellInfo` such as `haskell_library` or
`haskell_binary`.
""",
        ),
    },
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)
"""Exports a Haskell library as a CC library.

Given a [haskell_library](#haskell_library) or
[haskell_binary](#haskell_binary) input, outputs the shared object files
produced as well as the object files it depends on directly and
transitively. This is very useful if you want to link in a Haskell shared
library from `cc_library`.

There is a caveat: this will not provide any shared libraries that
aren't explicitly given to it. This means that if you're using
`prebuilt_dependencies` and relying on GHC to provide those objects,
they will not be present here. You will have to provide those
separately to your `cc_library`. If you're getting
`prebuilt_dependencies` from your toolchain, you will likely want to
extract those and pass them in as well.

*This rule is deprecated.*

Example:
  ```bzl
  haskell_library(
    name = "my-lib",
    ...
  )

  cc_haskell_import(
    name = "my-lib-objects",
    dep = ":my-lib",
  )

  cc_library(
    name = "my-cc",
    srcs = ["main.c", ":my-lib-objects"],
  )
  ```

[bazel-cpp-sandwich]: https://github.com/bazelbuild/bazel/issues/2163
"""
