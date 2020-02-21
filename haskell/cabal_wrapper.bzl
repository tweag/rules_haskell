load(":private/context.bzl", "haskell_context", "render_env")
load(":cc.bzl", "cc_interop_info")
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")
load("@rules_python//python:defs.bzl", "py_binary")

def _cabal_wrapper_impl(ctx):
    hs = haskell_context(ctx)
    hs_toolchain = ctx.toolchains["@rules_haskell//haskell:toolchain"]
    cc_toolchain = ctx.attr._cc_toolchain[cc_common.CcToolchainInfo]

    # If running on darwin but XCode is not installed (i.e., only the Command
    # Line Tools are available), then Bazel will make ar_executable point to
    # "/usr/bin/libtool". Since we call ar directly, override it.
    # TODO: remove this if Bazel fixes its behavior.
    # Upstream ticket: https://github.com/bazelbuild/bazel/issues/5127.
    ar = cc_toolchain.ar_executable
    if ar.find("libtool") >= 0:
        ar = "/usr/bin/ar"

    cabal_wrapper_tpl = ctx.file._cabal_wrapper_tpl
    cabal_wrapper = hs.actions.declare_file("cabal_wrapper.py")
    hs.actions.expand_template(
        template = cabal_wrapper_tpl,
        output = cabal_wrapper,
        is_executable = True,
        substitutions = {
            "%{ghc}": hs.tools.ghc.path,
            "%{ghc_pkg}": hs.tools.ghc_pkg.path,
            "%{runghc}": hs.tools.runghc.path,
            "%{ar}": ar,
            "%{cc}": hs_toolchain.cc_wrapper.executable.path,
            "%{strip}": cc_toolchain.strip_executable,
            "%{is_windows}": str(hs.toolchain.is_windows),
            "%{workspace}": ctx.workspace_name,
        },
    )
    return [DefaultInfo(
        files = depset([cabal_wrapper]),
        runfiles = hs.toolchain.cc_wrapper.runfiles.merge(ctx.runfiles(
            transitive_files = cc_toolchain.all_files,
            collect_data = True,
        )),
    )]

_cabal_wrapper = rule(
    implementation = _cabal_wrapper_impl,
    attrs = {
        "_cabal_wrapper_tpl": attr.label(
            allow_single_file = True,
            default = Label("@rules_haskell//haskell:private/cabal_wrapper.py.tpl"),
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
    toolchains = [
        "@bazel_tools//tools/cpp:toolchain_type",
        "@rules_haskell//haskell:toolchain",
    ],
    fragments = ["cpp"],
)

def cabal_wrapper(name, **kwargs):
    _cabal_wrapper(
        name = name + ".py",
    )
    py_binary(
        name = name,
        srcs = [name + ".py"],
        srcs_version = "PY3",
        python_version = "PY3",
        deps = [
            "@bazel_tools//tools/python/runfiles",
        ],
        **kwargs
    )
