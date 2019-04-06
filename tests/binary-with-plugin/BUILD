load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "ghc_plugin",
    "haskell_import",
    "haskell_library",
    "haskell_test",
)

package(default_testonly = 1)

config_setting(
    name = "debug_build",
    values = {
        "compilation_mode": "dbg",
    },
)

haskell_import(name = "base")

haskell_import(name = "ghc")

haskell_library(
    name = "plugin-lib",
    srcs = ["Plugin.hs"],
    deps = [
        ":base",
        ":ghc",
    ],
)

ghc_plugin(
    name = "plugin",
    args = ["$(JAVABASE)/bin/javac"],
    module = "Plugin",
    toolchains = ["@bazel_tools//tools/jdk:current_java_runtime"],
    deps = [":plugin-lib"],
)

haskell_test(
    name = "binary-with-plugin",
    srcs = ["Main.hs"],
    plugins = select({
        # XXX If profiling is enabled, ignore the plugin because of
        # https://gitlab.haskell.org/ghc/ghc/issues/14335.
        ":debug_build": [],
        "//conditions:default": [":plugin"],
    }),
    visibility = ["//visibility:public"],
    deps = [":base"],
)
