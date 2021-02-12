load(":private/context.bzl", "haskell_context", "render_env")
load(":cc.bzl", "cc_interop_info", "ghc_cc_program_args")
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")
load("@rules_python//python:defs.bzl", "py_binary")

def _cabal_wrapper_impl(ctx):
    cabal_wrapper_tpl = ctx.file._cabal_wrapper_tpl
    cabal_wrapper = ctx.actions.declare_file("cabal_wrapper.py")
    ctx.actions.expand_template(
        template = cabal_wrapper_tpl,
        output = cabal_wrapper,
        is_executable = True,
        substitutions = {
        },
    )
    return [DefaultInfo(
        files = depset([cabal_wrapper]),
    )]

_cabal_wrapper = rule(
    implementation = _cabal_wrapper_impl,
    attrs = {
        "_cabal_wrapper_tpl": attr.label(
            allow_single_file = True,
            default = Label("@rules_haskell//haskell:private/cabal_wrapper.py.tpl"),
        ),
    },
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
