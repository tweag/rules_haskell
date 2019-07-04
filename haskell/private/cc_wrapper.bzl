load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")
load("@bazel_tools//tools/build_defs/cc:action_names.bzl", "ACTION_NAMES")
load("@os_info//:os_info.bzl", "is_linux")

def _cc_wrapper_impl(ctx):
    cc_toolchain = find_cpp_toolchain(ctx)
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )
    cc = cc_common.get_tool_for_action(
        feature_configuration = feature_configuration,
        action_name = ACTION_NAMES.c_compile,
    )
    cc_wrapper = ctx.actions.declare_file(ctx.label.name + ".py")
    ctx.actions.expand_template(
        template = ctx.file.template,
        output = cc_wrapper,
        is_executable = True,
        substitutions = {
            "{:cc:}": cc,
        },
    )
    return [DefaultInfo(files = depset([cc_wrapper]))]

_cc_wrapper = rule(
    implementation = _cc_wrapper_impl,
    attrs = {
        "template": attr.label(
            allow_single_file = True,
            default = Label("@rules_haskell//haskell:private/cc_wrapper.py.tpl"),
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
    fragments = ["cpp"],
)

def cc_wrapper(name, template = None, **kwargs):
    _cc_wrapper(
        name = name + "-source",
        template = template,
    )
    native.py_binary(
        name = name,
        srcs = [name + "-source"],
        main = name + "-source.py",
        python_version = "PY3",
        deps = [
            "@bazel_tools//tools/python/runfiles",
        ],
        **kwargs
    )
