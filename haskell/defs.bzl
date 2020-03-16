"""Core Haskell rules"""

load(
    ":haddock.bzl",
    _haskell_doc = "haskell_doc",
    _haskell_doc_aspect = "haskell_doc_aspect",
)
load(
    ":private/haskell_impl.bzl",
    _haskell_binary_impl = "haskell_binary_impl",
    _haskell_import_impl = "haskell_import_impl",
    _haskell_library_impl = "haskell_library_impl",
    _haskell_test_impl = "haskell_test_impl",
    _haskell_toolchain_library_impl = "haskell_toolchain_library_impl",
)
load(
    ":repl.bzl",
    _haskell_repl = "haskell_repl",
    _haskell_repl_aspect = "haskell_repl_aspect",
)
load(":private/cc_libraries.bzl", "haskell_cc_libraries_aspect")

# For re-exports:
load(
    ":toolchain.bzl",
    _haskell_register_toolchains = "rules_haskell_toolchains",
    _haskell_toolchain = "haskell_toolchain",
)
load(
    ":plugins.bzl",
    _ghc_plugin = "ghc_plugin",
)

# NOTE: Documentation needs to be added to the wrapper macros below.
#   Currently it is not possible to automatically inherit rule documentation in
#   wrapping macros. See https://github.com/bazelbuild/stardoc/issues/27
_haskell_common_attrs = {
    "src_strip_prefix": attr.string(),
    "srcs": attr.label_list(
        allow_files = [".hs", ".hsc", ".lhs", ".hs-boot", ".lhs-boot", ".h"],
    ),
    "extra_srcs": attr.label_list(
        allow_files = True,
    ),
    "deps": attr.label_list(
        aspects = [haskell_cc_libraries_aspect],
    ),
    "data": attr.label_list(
        allow_files = True,
    ),
    "compiler_flags": attr.string_list(),
    "repl_ghci_args": attr.string_list(),
    "runcompile_flags": attr.string_list(),
    "plugins": attr.label_list(
        aspects = [haskell_cc_libraries_aspect],
    ),
    "tools": attr.label_list(
        cfg = "host",
    ),
    "_ghci_script": attr.label(
        allow_single_file = True,
        default = Label("@rules_haskell//haskell:assets/ghci_script"),
    ),
    "_ghci_repl_wrapper": attr.label(
        allow_single_file = True,
        default = Label("@rules_haskell//haskell:private/ghci_repl_wrapper.sh"),
    ),
    "_ls_modules": attr.label(
        executable = True,
        cfg = "host",
        default = Label("@rules_haskell//haskell:ls_modules"),
    ),
    "_version_macros": attr.label(
        executable = True,
        cfg = "host",
        default = Label("@rules_haskell//haskell:version_macros"),
    ),
    "_cc_toolchain": attr.label(
        default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
    ),
    "_ghc_wrapper": attr.label(
        executable = True,
        cfg = "host",
        default = Label("@rules_haskell//haskell:ghc_wrapper"),
    ),
    "worker": attr.label(
        default = None,
        executable = True,
        cfg = "host",
    ),
}

def _mk_binary_rule(**kwargs):
    """Generate a rule that compiles a binary.

    This is useful to create variations of a Haskell binary compilation
    rule without having to copy and paste the actual `rule` invocation.

    Args:
      **kwargs: Any additional keyword arguments to pass to `rule`.

    Returns:
      Rule: Haskell binary compilation rule.
    """

    is_test = kwargs.get("test", False)

    # NOTE: Documentation needs to be added to the wrapper macros below.
    #   Currently it is not possible to automatically inherit rule documentation
    #   in wrapping macros. See https://github.com/bazelbuild/stardoc/issues/27
    attrs = dict(
        _haskell_common_attrs,
        linkstatic = attr.bool(
            default = True,
        ),
        main_function = attr.string(
            default = "Main.main",
        ),
        version = attr.string(),
    )

    # Tests have an extra fields regarding code coverage.
    #
    # NOTE: Documentation needs to be added to the wrapper macros below.
    #   Currently it is not possible to automatically inherit rule documentation
    #   in wrapping macros. See https://github.com/bazelbuild/stardoc/issues/27
    if is_test:
        attrs.update({
            "expected_covered_expressions_percentage": attr.int(
                default = -1,
            ),
            "expected_uncovered_expression_count": attr.int(
                default = -1,
            ),
            "strict_coverage_analysis": attr.bool(
                default = False,
            ),
            "coverage_report_format": attr.string(
                default = "text",
            ),
            "experimental_coverage_source_patterns": attr.string_list(
                default = ["//..."],
            ),
            "_coverage_wrapper_template": attr.label(
                allow_single_file = True,
                default = Label("@rules_haskell//haskell:private/coverage_wrapper.sh.tpl"),
            ),
            "_bash_runfiles": attr.label(
                allow_single_file = True,
                default = Label("@bazel_tools//tools/bash/runfiles:runfiles"),
            ),
        })

    return rule(
        # If _mk_binary_rule was called with test = True, we want to use the test binary implementation
        _haskell_test_impl if is_test else _haskell_binary_impl,
        executable = True,
        attrs = attrs,
        outputs = {
            "runghc": "%{name}@runghc",
        },
        toolchains = [
            "@bazel_tools//tools/cpp:toolchain_type",
            "@rules_haskell//haskell:toolchain",
            "@rules_sh//sh/posix:toolchain_type",
        ],
        fragments = ["cpp"],
        **kwargs
    )

_haskell_test = _mk_binary_rule(test = True)

_haskell_binary = _mk_binary_rule()

_haskell_library = rule(
    _haskell_library_impl,
    # NOTE: Documentation needs to be added to the wrapper macros below.
    #   Currently it is not possible to automatically inherit rule documentation
    #   in wrapping macros. See https://github.com/bazelbuild/stardoc/issues/27
    attrs = dict(
        _haskell_common_attrs,
        hidden_modules = attr.string_list(),
        reexported_modules = attr.label_keyed_string_dict(),
        exports = attr.label_list(
            default = [],
            aspects = [haskell_cc_libraries_aspect],
        ),
        linkstatic = attr.bool(
            default = False,
        ),
        package_name = attr.string(),
        version = attr.string(),
    ),
    outputs = {
        "runghc": "%{name}@runghc",
    },
    toolchains = [
        "@bazel_tools//tools/cpp:toolchain_type",
        "@rules_haskell//haskell:toolchain",
        "@rules_sh//sh/posix:toolchain_type",
    ],
    fragments = ["cpp"],
)

def _haskell_worker_wrapper(rule_type, **kwargs):
    defaults = dict(
        worker = select({
            "@rules_haskell//haskell:use_worker": Label("@rules_haskell//tools/worker:bin"),
            "//conditions:default": None,
        }),
    )
    defaults.update(kwargs)

    if rule_type == "binary":
        _haskell_binary(**defaults)
    elif rule_type == "test":
        _haskell_test(**defaults)
    elif rule_type == "library":
        _haskell_library(**defaults)

def haskell_binary(
        name,
        src_strip_prefix = "",
        srcs = [],
        extra_srcs = [],
        deps = [],
        data = [],
        compiler_flags = [],
        repl_ghci_args = [],
        runcompile_flags = [],
        plugins = [],
        tools = [],
        worker = None,
        linkstatic = True,
        main_function = "Main.main",
        version = None,
        **kwargs):
    """Build an executable from Haskell source.

    Every `haskell_binary` target also defines an optional REPL target that is
    not built by default, but can be built on request. The name of the REPL
    target is the same as the name of binary with `"@repl"` added at the end.
    For example, the target above also defines `main@repl`.

    You can call the REPL like this (requires Bazel 0.15 or later):

    ```
    $ bazel run //:hello@repl
    ```

    ### Examples

      ```bzl
      haskell_binary(
          name = "hello",
          srcs = ["Main.hs", "Other.hs"],
          deps = ["//lib:some_lib"]
      )
      ```

    Args:
      name: A unique name for this rule.
      src_strip_prefix: DEPRECATED. Attribute has no effect.
      srcs: Haskell source files.
      extra_srcs: Extra (non-Haskell) source files that will be needed at compile time (e.g. by Template Haskell).
      deps: List of other Haskell libraries to be linked to this target.
      data: See [Bazel documentation](https://docs.bazel.build/versions/master/be/common-definitions.html#common.data).,
      compiler_flags: Flags to pass to Haskell compiler. Subject to Make variable substitution.
      repl_ghci_args: Arbitrary extra arguments to pass to GHCi. This extends `compiler_flags` and `repl_ghci_args` from the toolchain. Subject to Make variable substitution.,
      runcompile_flags: Arbitrary extra arguments to pass to runghc. This extends `compiler_flags` and `repl_ghci_args` from the toolchain. Subject to Make variable substitution.
      plugins: Compiler plugins to use during compilation.
      tools: Extra tools needed at compile-time, like preprocessors.
      worker: Experimental. Worker binary employed by Bazel's persistent worker mode. See [use-cases documentation](https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#persistent-worker-mode-experimental).
      linkstatic: Link dependencies statically wherever possible. Some system libraries may still be linked dynamically, as are libraries for which there is no static library. So the resulting executable will still be dynamically linked, hence only mostly static.
      main_function: A function with type `IO _`, either the qualified name of a function from any module or the bare name of a function from a `Main` module. It is also possible to give the qualified name of any module exposing a `main` function.
      version: Executable version. If this is specified, CPP version macros will be generated for this build.
      **kwargs: Common rule attributes. See [Bazel documentation](https://docs.bazel.build/versions/master/be/common-definitions.html#common-attributes).
    """
    _haskell_worker_wrapper(
        "binary",
        name = name,
        src_strip_prefix = src_strip_prefix,
        srcs = srcs,
        extra_srcs = extra_srcs,
        deps = deps,
        data = data,
        compiler_flags = compiler_flags,
        repl_ghci_args = repl_ghci_args,
        runcompile_flags = runcompile_flags,
        plugins = plugins,
        tools = tools,
        worker = worker,
        linkstatic = linkstatic,
        main_function = main_function,
        version = version,
        **kwargs
    )

    repl_kwargs = {
        attr: kwargs[attr]
        for attr in ["testonly", "tags"]
        if attr in kwargs
    }
    native.alias(
        # XXX Temporary backwards compatibility hack. Remove eventually.
        # See https://github.com/tweag/rules_haskell/pull/460.
        name = "%s-repl" % name,
        actual = "%s@repl" % name,
        **repl_kwargs
    )
    haskell_repl(
        name = "%s@repl" % name,
        deps = [name],
        experimental_from_source = [":%s" % name],
        repl_ghci_args = repl_ghci_args,
        **repl_kwargs
    )

def haskell_test(
        name,
        src_strip_prefix = "",
        srcs = [],
        extra_srcs = [],
        deps = [],
        data = [],
        compiler_flags = [],
        repl_ghci_args = [],
        runcompile_flags = [],
        plugins = [],
        tools = [],
        worker = None,
        linkstatic = True,
        main_function = "Main.main",
        version = None,
        expected_covered_expressions_percentage = -1,
        expected_uncovered_expression_count = -1,
        strict_coverage_analysis = False,
        coverage_report_format = "text",
        experimental_coverage_source_patterns = ["//..."],
        **kwargs):
    """Build a test suite.

    Additionally, it accepts [all common bazel test rule
    fields][bazel-test-attrs]. This allows you to influence things like
    timeout and resource allocation for the test.

    [bazel-test-attrs]: https://docs.bazel.build/versions/master/be/common-definitions.html#common-attributes-tests

    Args:
      name: A unique name for this rule.
      src_strip_prefix: DEPRECATED. Attribute has no effect.
      srcs: Haskell source files.
      extra_srcs: Extra (non-Haskell) source files that will be needed at compile time (e.g. by Template Haskell).
      deps: List of other Haskell libraries to be linked to this target.
      data: See [Bazel documentation](https://docs.bazel.build/versions/master/be/common-definitions.html#common.data).,
      compiler_flags: Flags to pass to Haskell compiler. Subject to Make variable substitution.
      repl_ghci_args: Arbitrary extra arguments to pass to GHCi. This extends `compiler_flags` and `repl_ghci_args` from the toolchain. Subject to Make variable substitution.,
      runcompile_flags: Arbitrary extra arguments to pass to runghc. This extends `compiler_flags` and `repl_ghci_args` from the toolchain. Subject to Make variable substitution.
      plugins: Compiler plugins to use during compilation.
      tools: Extra tools needed at compile-time, like preprocessors.
      worker: Experimental. Worker binary employed by Bazel's persistent worker mode. See [use-cases documentation](https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#persistent-worker-mode-experimental).
      linkstatic: Link dependencies statically wherever possible. Some system libraries may still be linked dynamically, as are libraries for which there is no static library. So the resulting executable will still be dynamically linked, hence only mostly static.
      main_function: A function with type `IO _`, either the qualified name of a function from any module or the bare name of a function from a `Main` module. It is also possible to give the qualified name of any module exposing a `main` function.
      version: Executable version. If this is specified, CPP version macros will be generated for this build.
      expected_covered_expressions_percentage: The expected percentage of expressions covered by testing.
      expected_uncovered_expression_count: The expected number of expressions which are not covered by testing.
      strict_coverage_analysis: Requires that the coverage metric is matched exactly, even doing better than expected is not allowed.
      coverage_report_format: The format to output the coverage report in.

        Supported values: "text", "html". Default: "text".

        Report can be seen in the testlog XML file, or by setting --test_output=all when running bazel coverage.
      experimental_coverage_source_patterns: The path patterns specifying which targets to analyze for test coverage metrics.

          Wild-card targets such as //... or //:all are allowed. The paths must be relative to the workspace, which means they must start with "//".

          Note, this attribute may leave experimental status depending on the outcome of https://github.com/bazelbuild/bazel/issues/7763.
      **kwargs: Common rule attributes. See [Bazel documentation](https://docs.bazel.build/versions/master/be/common-definitions.html#common-attributes).
    """
    _haskell_worker_wrapper(
        "test",
        name = name,
        src_strip_prefix = src_strip_prefix,
        srcs = srcs,
        extra_srcs = extra_srcs,
        deps = deps,
        data = data,
        compiler_flags = compiler_flags,
        repl_ghci_args = repl_ghci_args,
        runcompile_flags = runcompile_flags,
        plugins = plugins,
        tools = tools,
        worker = worker,
        linkstatic = linkstatic,
        main_function = main_function,
        version = version,
        expected_covered_expressions_percentage = expected_covered_expressions_percentage,
        expected_uncovered_expression_count = expected_uncovered_expression_count,
        strict_coverage_analysis = strict_coverage_analysis,
        coverage_report_format = coverage_report_format,
        experimental_coverage_source_patterns = experimental_coverage_source_patterns,
        **kwargs
    )

    repl_kwargs = {
        attr: kwargs[attr]
        for attr in ["tags"]
        if attr in kwargs
    }
    native.alias(
        # XXX Temporary backwards compatibility hack. Remove eventually.
        # See https://github.com/tweag/rules_haskell/pull/460.
        name = "%s-repl" % name,
        actual = "%s@repl" % name,
        testonly = kwargs.get("testonly", True),
        **repl_kwargs
    )
    haskell_repl(
        name = "%s@repl" % name,
        deps = [name],
        experimental_from_source = [":%s" % name],
        repl_ghci_args = repl_ghci_args,
        testonly = kwargs.get("testonly", True),
        **repl_kwargs
    )

def haskell_library(
        name,
        src_strip_prefix = "",
        srcs = [],
        extra_srcs = [],
        deps = [],
        data = [],
        compiler_flags = [],
        repl_ghci_args = [],
        runcompile_flags = [],
        plugins = [],
        tools = [],
        worker = None,
        hidden_modules = [],
        reexported_modules = {},
        exports = [],
        linkstatic = False,
        package_name = "",
        version = "",
        **kwargs):
    """Build a library from Haskell source.

    Every `haskell_library` target also defines an optional REPL target that is
    not built by default, but can be built on request. It works the same way as
    for `haskell_binary`.

    ### Examples

      ```bzl
      haskell_library(
          name = "hello-lib",
          srcs = glob(["src/**/*.hs"]),
          src_strip_prefix = "src",
          deps = [
              "//hello-sublib:lib",
          ],
          reexported_modules = {
              "//hello-sublib:lib": "Lib1 as HelloLib1, Lib2",
          },
      )
      ```

    Args:
      name: A unique name for this rule.
      src_strip_prefix: DEPRECATED. Attribute has no effect.
      srcs: Haskell source files.
      extra_srcs: Extra (non-Haskell) source files that will be needed at compile time (e.g. by Template Haskell).
      deps: List of other Haskell libraries to be linked to this target.
      data: See [Bazel documentation](https://docs.bazel.build/versions/master/be/common-definitions.html#common.data).,
      compiler_flags: Flags to pass to Haskell compiler. Subject to Make variable substitution.
      repl_ghci_args: Arbitrary extra arguments to pass to GHCi. This extends `compiler_flags` and `repl_ghci_args` from the toolchain. Subject to Make variable substitution.,
      runcompile_flags: Arbitrary extra arguments to pass to runghc. This extends `compiler_flags` and `repl_ghci_args` from the toolchain. Subject to Make variable substitution.
      plugins: Compiler plugins to use during compilation.
      tools: Extra tools needed at compile-time, like preprocessors.
      worker: Experimental. Worker binary employed by Bazel's persistent worker mode. See [use-cases documentation](https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#persistent-worker-mode-experimental).
      hidden_modules: Modules that should be unavailable for import by dependencies.
      reexported_modules: A dictionary mapping dependencies to module reexports that should be available for import by dependencies.
      exports: A list of other haskell libraries that will be transparently added as a dependency to every downstream rule
      linkstatic: Create a static library, not both a static and a shared library.
      package_name: Library name used in version macro generation. Only used
        if the version attribute is defined, see version attribute
        documentation. Optional, defaults to target name.
      version: Library version. Not normally necessary unless to build a library
        originally defined as a Cabal package. If this is specified, CPP version macro will be generated.
      **kwargs: Common rule attributes. See [Bazel documentation](https://docs.bazel.build/versions/master/be/common-definitions.html#common-attributes).
    """
    _haskell_worker_wrapper(
        "library",
        name = name,
        src_strip_prefix = src_strip_prefix,
        srcs = srcs,
        extra_srcs = extra_srcs,
        deps = deps,
        data = data,
        compiler_flags = compiler_flags,
        repl_ghci_args = repl_ghci_args,
        runcompile_flags = runcompile_flags,
        plugins = plugins,
        tools = tools,
        worker = worker,
        hidden_modules = hidden_modules,
        reexported_modules = reexported_modules,
        exports = exports,
        linkstatic = linkstatic,
        package_name = package_name,
        version = version,
        **kwargs
    )

    repl_kwargs = {
        attr: kwargs[attr]
        for attr in ["testonly", "tags"]
        if attr in kwargs
    }
    native.alias(
        # XXX Temporary backwards compatibility hack. Remove eventually.
        # See https://github.com/tweag/rules_haskell/pull/460.
        name = "%s-repl" % name,
        actual = "%s@repl" % name,
        **repl_kwargs
    )
    haskell_repl(
        name = "%s@repl" % name,
        deps = [name],
        experimental_from_source = [":%s" % name],
        repl_ghci_args = repl_ghci_args,
        **repl_kwargs
    )

haskell_import = rule(
    _haskell_import_impl,
    attrs = {
        "id": attr.string(),
        "version": attr.string(),
        "deps": attr.label_list(),
        "static_libraries": attr.label_list(allow_files = [".a"]),
        "shared_libraries": attr.label_list(allow_files = True),
        "static_profiling_libraries": attr.label_list(allow_files = ["_p.a"]),
        "linkopts": attr.string_list(),
        "hdrs": attr.label_list(allow_files = True),
        "includes": attr.string_list(),
        "haddock_interfaces": attr.label_list(allow_files = True),
        "haddock_html": attr.label(allow_single_file = True),
        "_version_macros": attr.label(
            executable = True,
            cfg = "host",
            default = Label("@rules_haskell//haskell:version_macros"),
        ),
    },
)

haskell_toolchain_library = rule(
    _haskell_toolchain_library_impl,
    attrs = dict(
        package = attr.string(
            doc = "The name of a GHC package not built by Bazel. Defaults to the name of the rule.",
        ),
        _toolchain_libraries = attr.label(
            default = Label("@rules_haskell//haskell:toolchain-libraries"),
        ),
    ),
    toolchains = [
        "@rules_haskell//haskell:toolchain",
    ],
    doc = """\
Import packages that are prebuilt outside of Bazel.

### Examples

  ```bzl
  haskell_toolchain_library(
      name = "base_pkg",
      package = "base",
  )

  haskell_library(
      name = "hello-lib",
      srcs = ["Lib.hs"],
      deps = [
          ":base_pkg",
          "//hello-sublib:lib",
      ],
  )
  ```

Use this rule to make dependencies that are prebuilt (supplied as part
of the compiler toolchain) available as targets.
""",
)

haskell_doc = _haskell_doc

haskell_doc_aspect = _haskell_doc_aspect

haskell_register_toolchains = _haskell_register_toolchains

haskell_repl = _haskell_repl

haskell_repl_aspect = _haskell_repl_aspect

haskell_toolchain = _haskell_toolchain

ghc_plugin = _ghc_plugin
