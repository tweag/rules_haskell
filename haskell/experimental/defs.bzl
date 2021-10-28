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
        "src": attr.label(
            # TODO[AH] How to handle .hsc files?
            # TODO[AH] Do we need .h files in here?
            allow_single_file = [".hs", ".lhs", ".hs-boot", ".lhs-boot"],  #, ".hsc", ".h"],
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
        plugins = [],
        tools = [],
        worker = None,
        **kwargs):
    """Compile a module from Haskell source.

    Note: This rule is experimental and not ready for production, yet.

    ### Examples

      ```bzl
      haskell_module(
          name = "Example.Module",
          src = "src/Example/Module.hs",
          src_strip_prefix = "src",
          deps = [
              "//:Another.Module",
              "//:some-library",
          ],
      )
      ```

    Args:
      name: A unique name for this rule.
      src_strip_prefix: Prefix before the path matches the module name.
        This is used as an import search for the Haskell compiler.
        Values starting with `/` are relative to the workspace root,
        other paths are relative to the package.
      src: The Haskell source file.
      extra_srcs: Extra (non-Haskell) source files that will be needed at compile time (e.g. by Template Haskell).
      deps: List of other Haskell modules or libraries needed to compile this module.
      data: See [Bazel documentation](https://docs.bazel.build/versions/master/be/common-definitions.html#common.data).,
      ghcopts: Flags to pass to Haskell compiler. Subject to Make variable substitution.
      plugins: Compiler plugins to use during compilation. (Not implemented, yet)
      tools: Extra tools needed at compile-time, like preprocessors. (Not implemented, yet)
      worker: Experimental. Worker binary employed by Bazel's persistent worker mode. See [use-cases documentation](https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#persistent-worker-mode-experimental). (Not implemented, yet)
      **kwargs: Common rule attributes. See [Bazel documentation](https://docs.bazel.build/versions/master/be/common-definitions.html#common-attributes).
    """
    _haskell_module(
        name = name,
        src = src,
        extra_srcs = extra_srcs,
        deps = deps,
        ghcopts = ghcopts,
        plugins = plugins,
        tools = tools,
        #worker = worker,
        **kwargs
    )
