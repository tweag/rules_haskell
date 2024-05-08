load("@rules_cc//cc:action_names.bzl", "ACTION_NAMES")
load("@rules_cc//cc:find_cc_toolchain.bzl", "find_cc_toolchain", "use_cc_toolchain")
load("@rules_python//python:defs.bzl", "py_binary")

# Note [On configuring the cc_wrapper]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# The cc_wrapper provides a thin compatibility layer between GHC and
# the cc-toolchain that Bazel provides. It is a script that GHC will
# invoke as the C compiler (pgmc) and that itself will invoke the C
# compiler provided by Bazel's cc-toolchain. Therefore cc_wrapper only
# needs the cc-toolchain at runtime, i.e. when building Haskell
# binaries. More importantly, in case of cross-compilation, cc_wrapper
# needs a cc-toolchain that targets the platform that we are building
# the Haskell binary for.
#
# However, we do not have control over the arguments that GHC passes
# to the cc_wrapper in all cases. Meaning, we cannot use command line
# arguments to configure cc_wrapper to use a particular cc-toolchain.
# Instead, we use environment variables to pass all relevant
# information about the cc-toolchain to cc_wrapper.
#
# Unfortunately, environment variables are not available in the IDE
# use-case, because we cannot specify environment variables at the
# hie-bios interface. To work around this limitation, we configure a
# fall-back cc-toolchain as a build-time dependency to cc_wrapper.
#
# Note, Bazel will not resolve the desired cc-toolchain at this
# build-time dependency in case of cross-compilation. The cc_wrapper
# is a dependency to the hs-toolchain and Bazel will build it to
# target the build platform (execute) instead of the ultimate target
# platform (target). Therefore, the fall-back cc-toolchain will not
# target the cross-platform and should not be used for any build
# actions.
#

def _cc_wrapper_impl(ctx):
    cc_toolchain = find_cc_toolchain(ctx)
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
    cc_wrapper = ctx.actions.declare_file(ctx.label.name)
    ctx.actions.expand_template(
        template = ctx.file.template,
        output = cc_wrapper,
        is_executable = True,
        substitutions = {
            "{:cc:}": cc,
            "{:cpu:}": cc_toolchain.cpu,
            "{:workspace:}": ctx.workspace_name,
            "{:platform:}": ctx.attr.platform,
        },
    )
    return [DefaultInfo(
        files = depset([cc_wrapper]),
        runfiles = ctx.runfiles(
            transitive_files = cc_toolchain.all_files,
            collect_data = True,
        ),
    )]

_cc_wrapper = rule(
    implementation = _cc_wrapper_impl,
    attrs = {
        "template": attr.label(
            allow_single_file = True,
        ),
        "platform": attr.string(),
        # See note [On configuring the cc_wrapper]
        #
        # TODO: Consider using execution groups to transition the toolchain
        # to the target platform.
        "_cc_toolchain": attr.label(
            default = Label("@rules_cc//cc:current_cc_toolchain"),
        ),
    },
    fragments = ["cpp"],
    toolchains = use_cc_toolchain(),
)

def cc_wrapper(name, **kwargs):
    _cc_wrapper(
        name = name + ".py",
        template = "@rules_haskell//haskell:private/cc_wrapper.py.tpl",
        platform = select({
            "@rules_haskell//haskell/platforms:darwin": "darwin",
            "@rules_haskell//haskell/platforms:mingw32": "windows",
            "//conditions:default": "linux",
        }),
    )
    _cc_wrapper(
        name = name + ".sh",
        template = "@rules_haskell//haskell:private/cc_wrapper_windows.sh.tpl",
        platform = select({
            "@rules_haskell//haskell/platforms:darwin": "darwin",
            "@rules_haskell//haskell/platforms:mingw32": "windows",
            "//conditions:default": "linux",
        }),
    )
    py_binary(
        name = name + "-python",
        srcs = [name + ".py"],
        python_version = "PY3",
        main = name + ".py",
        deps = [
            "@rules_python//python/runfiles",
        ],
        **kwargs
    )

    # This is a workaround for py_binary being too slow on Windows.
    # See https://github.com/bazelbuild/bazel/issues/8981
    # In principle the python cc_wrapper would be sufficient for all platforms,
    # however, execution is too slow on Windows to be practical.
    native.sh_binary(
        name = name + "-bash",
        srcs = [name + ".sh"],
        deps = [
            "@bazel_tools//tools/bash/runfiles",
        ],
        **kwargs
    )
    native.alias(
        name = name,
        actual = select({
            "@rules_haskell//haskell/platforms:mingw32": name + "-bash",
            "//conditions:default": name + "-python",
        }),
        **kwargs
    )
