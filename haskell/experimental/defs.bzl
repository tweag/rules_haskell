"""Experimental Haskell rules"""

load("//haskell/experimental:providers.bzl", "HaskellModuleInfo")
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
        "module_name": attr.string(),
        "extra_srcs": attr.label_list(
            allow_files = True,
        ),
        "deps": attr.label_list(),
        "cross_library_deps": attr.label_list(),
        "enable_th": attr.bool(),
        "ghcopts": attr.string_list(),
        "plugins": attr.label_list(
            aspects = [haskell_cc_libraries_aspect],
        ),
        "tools": attr.label_list(
            cfg = "host",
            allow_files = True,
        ),
        "_cc_toolchain": attr.label(
            default = Label("@rules_cc//cc:current_cc_toolchain"),
        ),
        "_ghc_wrapper": attr.label(
            executable = True,
            cfg = "host",
            default = Label("@rules_haskell//haskell:ghc_wrapper"),
        ),
    },
    toolchains = [
        "@rules_cc//cc:toolchain_type",
        "@rules_haskell//haskell:toolchain",
        "@rules_sh//sh/posix:toolchain_type",
    ],
    fragments = ["cpp"],
)

def haskell_module(
        name,
        src = None,
        extra_srcs = [],
        module_name = "",
        deps = [],
        cross_library_deps = [],
        enable_th = False,
        ghcopts = [],
        plugins = [],
        tools = [],
        **kwargs):
    """Declare a module and its dependencies on other modules.

    This allows library, binary, and test rules to do incremental builds when only a
    few modules are affected by a change.

    There are three types of dependencies. `deps` contains other modules included in the
    same enclosing library, binary, or test. `cross_library_deps` contains modules
    coming from other libraries, and these libraries need to be listed in the
    `narrowed_deps` attribute of the enclosing library, binary, or test. Finally,
    `haskell_module` depends on all the libraries in the `deps` attribute of the
    enclosing library, binary or test.

    Note: This rule is experimental and not ready for production, yet.

    ### Examples

      ```bzl
      haskell_module(
          name = "LibMod1",
          src = "src/LibMod1.hs",
          src_strip_prefix = "src",
          # Modules listed here need to come from the same
          # library as LibMod1.
          deps = [
              ":LibMod2",
          ],
      )

      haskell_module(
          name = "LibMod2",
          src = "src/LibMod2.hs",
          src_strip_prefix = "src",
          # If the module uses TH, set this to True. Otherwise object
          # files and shared libraries of dependencies won't be
          # available when building.
          enable_th = True,
      )

      haskell_library(
          name = "lib",
          # Must choose either one of srcs or modules
          # srcs = ...,
          modules = [
              # All modules to link into the binary need to be listed here
              "LibMod1",
              "LibMod2",
          ],
          # Any library dependencies of modules need to be listed here
          deps = [
              "//:base",
              "//:template-haskell",
          ],
      )

      haskell_module(
          name = "Example.Module",
          src = "src/Example/Module.hs",
          src_strip_prefix = "src",
          # Modules listed here need to come from libraries
          # listed in narrowed_deps
          cross_library_deps = [":LibMod1"],
          # Modules listed here need to come from the same
          # library as Example.Module.
          deps = [
              "//:Another.Module",
          ],
      )

      haskell_module(
          name = "Another.Module",
          src = "src/Another/Module.hs",
          src_strip_prefix = "src",
          cross_library_deps = [":LibMod2"],
      )

      haskell_binary(
          name = "haskellbin",
          modules = [
              "//:Example.Module",
              "//:Another.Module",
          ],
          deps = [
              "//:base",
              "//:template-haskell",
          ],
          # Any library dependencies of modules need to be listed here
          narrowed_deps = [":lib"]
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
                  This is merged with the extra_srcs attribute of rules that depend directly on this haskell_module rule.
      module_name: Use the given module name instead of trying to infer it from src and src_strip_prefix. This is
                   necessary when the src file is not named the same as the Haskell module.
      deps: List of other Haskell modules needed to compile this module. They need to be included in the `modules`
               attribute of the enclosing library, binary, or test.
               If the module depends on any libraries, they should be listed in the deps attribute of the library,
               binary, or test that depends on this module.
      cross_library_deps: List of other Haskell modules needed to compile this module that come from other libraries.
               They need to be included in the `modules` attribute of any library in the `narrowed_deps` attribute
               of the enclosing library, binary, or test
      enable_th: Exposes object files or libraries to the build action. This is necessary when the module uses
               Template Haskell. The libraries of narrowed deps are exposed instead of object files in profiling
               builds due to technical limitations.
      ghcopts: Flags to pass to Haskell compiler. Subject to Make variable substitution.
               This is merged with the ghcopts attribute of rules that depend directly on this haskell_module rule.
      plugins: Compiler plugins to use during compilation. (Not implemented, yet)
               This is merged with the plugins attribute of rules that depend directly on this haskell_module rule.
      tools: Extra tools needed at compile-time, like preprocessors. (Not implemented, yet)
             This is merged with the tools attribute of rules that depend directly on this haskell_module rule.
      **kwargs: Common rule attributes. See [Bazel documentation](https://docs.bazel.build/versions/master/be/common-definitions.html#common-attributes).
    """
    _haskell_module(
        name = name,
        src = src,
        extra_srcs = extra_srcs,
        module_name = module_name,
        deps = deps,
        cross_library_deps = cross_library_deps,
        enable_th = enable_th,
        ghcopts = ghcopts,
        plugins = plugins,
        tools = tools,
        **kwargs
    )
