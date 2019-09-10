_test_script_template = """#!/usr/bin/env bash
library_path={library_path}
is_windows={is_windows}
expected="bin/_solib_{cpu}"
if [[ "$is_windows" != 1 && ! $library_path =~ ^.*"/$expected/".*$ ]]; then
    echo "Expected library path containing directory '$expected'," >&2
    echo "but got: '$library_path'." >&2
    exit 1
fi
"""

def _solib_test_impl(ctx):
    # Write a dummy dynamic library. It will never be loaded, we're only
    # interested in the paths that Bazel generates.
    dynamic_library = ctx.actions.declare_file("lib{}.so".format(ctx.label.name))
    ctx.actions.write(
        content = "",
        is_executable = False,
        output = dynamic_library,
    )

    # XXX Workaround https://github.com/bazelbuild/bazel/issues/6874.
    # Should be find_cpp_toolchain() instead.
    cc_toolchain = ctx.attr._cc_toolchain[cc_common.CcToolchainInfo]
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )
    lib_to_link = cc_common.create_library_to_link(
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        dynamic_library = dynamic_library,
        cc_toolchain = cc_toolchain,
    )

    # Write the test script.
    test_script = ctx.actions.declare_file(ctx.label.name)
    ctx.actions.write(
        content = _test_script_template.format(
            cpu = cc_toolchain.cpu,
            library_path = lib_to_link.dynamic_library.path,
            is_windows = "1" if ctx.attr.is_windows else "",
        ),
        is_executable = True,
        output = test_script,
    )
    return [DefaultInfo(
        executable = test_script,
    )]

solib_test = rule(
    _solib_test_impl,
    attrs = {
        "is_windows": attr.bool(),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
    executable = True,
    fragments = ["cpp"],
    test = True,
)
"""Test that Bazel's solib directory matches our expectations.

The cc_wrapper used by rules_haskell (haskell/private/cc_wrapper.py.tpl)
assumes that Bazel generates symbolic links for dynamic libraries under a
directory called `bin/_solib_<cpu>` on Darwin and Linux. This rule generates a
test-case that fails if this assumption is not met.
"""
