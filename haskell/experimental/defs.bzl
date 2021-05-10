"""Experimental Haskell rules"""

load(
    "//haskell/experimental/private:module.bzl",
    _haskell_module_impl = "haskell_module_impl",
)
load("//haskell:private/cc_libraries.bzl", "haskell_cc_libraries_aspect")

_haskell_module = rule(
    _haskell_module_impl,
    # NOTE: Documentation needs to be added to the wrapper macros below.
    #   Currently it is not possible to automatically inherit rule documentation
    #   in wrapping macros. See https://github.com/bazelbuild/stardoc/issues/27
    attrs = {
        # TODO[AH] Merge with _haskell_common_attrs in //haskell:defs.bzl
        "src": attr.label(
            # TODO[AH] How to handle boot files?
            # TODO[AH] How to handle .hsc files?
            # TODO[AH] Do we need .h files in here?
            allow_single_file = [".hs", ".lhs"],  #, ".hs-boot", ".lhs-boot", ".hsc", ".h"],
            mandatory = True,
        ),
        "src_strip_prefix": attr.string(),
        "extra_srcs": attr.label_list(
            allow_files = True,
        ),
        "deps": attr.label_list(
            aspects = [haskell_cc_libraries_aspect],
        ),
        "ghcopts": attr.string_list(),
        #"repl_ghci_args": attr.string_list(),
        "plugins": attr.label_list(
            aspects = [haskell_cc_libraries_aspect],
        ),
        "tools": attr.label_list(
            cfg = "host",
            allow_files = True,
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
        "_ghc_wrapper": attr.label(
            executable = True,
            cfg = "host",
            default = Label("@rules_haskell//haskell:ghc_wrapper"),
        ),
        # TODO[AH] Suppport worker
    },
    toolchains = [
        "@bazel_tools//tools/cpp:toolchain_type",
        "@rules_haskell//haskell:toolchain",
        "@rules_sh//sh/posix:toolchain_type",
    ],
    fragments = ["cpp"],
)

def haskell_module(
        name,
        src = None,
        extra_srcs = [],
        deps = [],
        ghcopts = [],
        #repl_ghci_args = [],
        plugins = [],
        tools = [],
        worker = None,
        **kwargs):
    """Build a module from Haskell source.

    TODO[AH] Write API docs
    """
    _haskell_module(
        name = name,
        src = src,
        extra_srcs = extra_srcs,
        deps = deps,
        ghcopts = ghcopts,
        #repl_ghci_args = repl_ghci_args,
        plugins = plugins,
        tools = tools,
        #worker = worker,
        **kwargs
    )

    #repl_kwargs = {
    #    attr: kwargs[attr]
    #    for attr in ["testonly", "tags"]
    #    if attr in kwargs
    #}
    #haskell_repl(
    #    name = "%s@repl" % name,
    #    deps = [name],
    #    experimental_from_source = [":%s" % name],
    #    repl_ghci_args = [],
    #    **repl_kwargs
    #)
