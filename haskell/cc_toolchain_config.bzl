load(
    "@rules_cc//cc:action_names.bzl",
    "ACTION_NAMES",
)
load(
    "@rules_cc//cc:cc_toolchain_config_lib.bzl",
    "artifact_name_pattern",
    "feature",
    "flag_group",
    "flag_set",
    "tool_path",
)

def _impl(ctx):
    tool_paths = [
        tool_path(
            name = "ar",
            path = "mingw/bin/ar",
        ),
        tool_path(
            name = "compat-ld",
            path = "mingw/bin/ld",
        ),
        tool_path(
            name = "cpp",
            path = "mingw/bin/cpp",
        ),
        tool_path(
            name = "dwp",
            path = "mingw/bin/dwp",
        ),
        tool_path(
            name = "gcc",
            path = "mingw/bin/clang" if ctx.attr.is_clang else "mingw/bin/gcc",
        ),
        tool_path(
            name = "gcov",
            path = "mingw/bin/gcov",
        ),
        tool_path(
            name = "ld",
            path = "mingw/bin/ld",
        ),
        tool_path(
            name = "nm",
            path = "mingw/bin/nm",
        ),
        tool_path(
            name = "objcopy",
            path = "mingw/bin/objcopy",
        ),
        tool_path(
            name = "objdump",
            path = "mingw/bin/objdump",
        ),
        tool_path(
            name = "strip",
            path = "mingw/bin/strip",
        ),
    ]
    artifact_name_patterns = [
        artifact_name_pattern(
            category_name = "executable",
            prefix = "",
            extension = ".exe",
        ),
    ]
    default_link_flags_feature = feature(
        name = "default_link_flags",
        enabled = True,
        flag_sets = [
            flag_set(
                actions = [
                    ACTION_NAMES.cpp_link_executable,
                    ACTION_NAMES.cpp_link_dynamic_library,
                    ACTION_NAMES.cpp_link_nodeps_dynamic_library,
                ],
                flag_groups = [
                    flag_group(
                        flags = [
                            "-lstdc++",
                        ],
                    ),
                ],
            ),
        ],
    )
    return cc_common.create_cc_toolchain_config_info(
        ctx = ctx,
        toolchain_identifier = "ghc_windows_mingw64",
        host_system_name = "local",
        target_system_name = "local",
        target_cpu = "x64_windows",
        target_libc = "local",
        compiler = "ghc-mingw-gcc",
        abi_version = "local",
        abi_libc_version = "local",
        tool_paths = tool_paths,
        artifact_name_patterns = artifact_name_patterns,
        features = [default_link_flags_feature],
    )

cc_toolchain_config = rule(
    implementation = _impl,
    attrs = {
        "is_clang": attr.bool(default = False, mandatory = False),
    },
    provides = [CcToolchainConfigInfo],
)
