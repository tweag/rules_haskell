load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
    "haskell_test",
)
load("//tests:inline_tests.bzl", "sh_inline_test")

# test whether `linkstatic` works as expected
package(default_testonly = 1)

cc_library(
    name = "c-lib",
    srcs = ["c-lib.c"],
)

haskell_library(
    name = "HsLib",
    srcs = ["HsLib.hs"],
    deps = [
        "//tests/hackage:base",
    ],
)

haskell_test(
    name = "binary-static",
    srcs = ["Main.hs"],
    linkstatic = True,
    deps = [
        ":HsLib",
        ":c-lib",
        "//tests/hackage:base",
    ],
)

haskell_test(
    name = "binary-dynamic",
    srcs = ["Main.hs"],
    linkstatic = False,
    deps = [
        ":HsLib",
        ":c-lib",
        "//tests/hackage:base",
    ],
)

config_setting(
    name = "debug_build",
    values = {
        "compilation_mode": "dbg",
    },
)

# Ensure that linkstatic=True only links to static library targets.
sh_inline_test(
    name = "test-binary-static-symbols",
    size = "small",
    args = [
        "$(rootpath :binary-static)",
    ],
    data = [
        ":binary-static",
    ],
    script = """
    set -eo pipefail
    binary="$1"
    # Symbols are prefixed with underscore on MacOS but not on Linux.
    if nm -u "$binary" | grep -q "\<_\?value"; then
        echo "C library dependency not linked statically: ${binary}"
        exit 1
    fi
    if nm -u "$binary" | grep -q HsLib_value_closure; then
        echo "Haskell library dependency not linked statically ${binary}"
        exit 1
    fi
    """,
)

# Ensure that linkstatic=False only links to dynamic library targets.
sh_inline_test(
    name = "test-binary-dynamic-symbols",
    size = "small",
    args = [
        "$(rootpath :binary-dynamic)",
    ] + select({
        ":debug_build": ["dbg"],
        "//conditions:default": ["rls"],
    }),
    data = [
        ":binary-dynamic",
    ],
    script = """
    set -eo pipefail
    binary="$1"
    mode="$2"
    if [[ $mode = dbg ]]; then
        # Skip test in debug builds. Debug mode forces static linking.
        exit 0
    fi
    # Symbols are prefixed with underscore on MacOS but not on Linux.
    if ! nm -u "$binary" | grep -q "\<_\?value"; then
        echo "C library dependency not linked dynamically"
        exit 1
    fi
    if ! nm -u "$binary" | grep -q HsLib_value_closure; then
        echo "Haskell library dependency not linked dynamically"
        exit 1
    fi
    """,
)

test_suite(
    name = "binary-linkstatic-flag",
    tests = [
        ":test-binary-dynamic-symbols",
        ":test-binary-static-symbols",
    ],
    visibility = ["//visibility:public"],
)
