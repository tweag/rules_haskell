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

# For re-exports:
load(
    ":toolchain.bzl",
    _haskell_register_toolchains = "haskell_register_toolchains",
    _haskell_toolchain = "haskell_toolchain",
)
load(
    ":plugins.bzl",
    _ghc_plugin = "ghc_plugin",
)

_haskell_common_attrs = {
    "src_strip_prefix": attr.string(
        doc = "DEPRECATED. Attribute has no effect.",
    ),
    "srcs": attr.label_list(
        allow_files = [".hs", ".hsc", ".lhs", ".hs-boot", ".lhs-boot", ".h"],
        doc = "Haskell source files.",
    ),
    "extra_srcs": attr.label_list(
        allow_files = True,
        doc = "Extra (non-Haskell) source files that will be needed at compile time (e.g. by Template Haskell).",
    ),
    "deps": attr.label_list(
        doc = "List of other Haskell libraries to be linked to this target.",
    ),
    "data": attr.label_list(
        doc = "See [Bazel documentation](https://docs.bazel.build/versions/master/be/common-definitions.html#common.data).",
        allow_files = True,
    ),
    "compiler_flags": attr.string_list(
        doc = "Flags to pass to Haskell compiler. Subject to Make variable substitution.",
    ),
    "repl_ghci_args": attr.string_list(
        doc = "Arbitrary extra arguments to pass to GHCi. This extends `compiler_flags` and `repl_ghci_args` from the toolchain. Subject to Make variable substitution.",
    ),
    "runcompile_flags": attr.string_list(
        doc = "Arbitrary extra arguments to pass to runghc. This extends `compiler_flags` and `repl_ghci_args` from the toolchain. Subject to Make variable substitution.",
    ),
    "plugins": attr.label_list(
        doc = "Compiler plugins to use during compilation.",
    ),
    "tools": attr.label_list(
        cfg = "host",
        doc = "Extra tools needed at compile-time, like preprocessors.",
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
        doc = "Experimental. Worker binary employed by Bazel's persistent worker mode. See docs/haskell-use-cases.rst",
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

    attrs = dict(
        _haskell_common_attrs,
        linkstatic = attr.bool(
            default = True,
            doc = "Link dependencies statically wherever possible. Some system libraries may still be linked dynamically, as are libraries for which there is no static library. So the resulting executable will still be dynamically linked, hence only mostly static.",
        ),
        main_function = attr.string(
            default = "Main.main",
            doc = """A function with type `IO _`, either the qualified name of a function from any module or the bare name of a function from a `Main` module. It is also possible to give the qualified name of any module exposing a `main` function.""",
        ),
        version = attr.string(
            doc = "Executable version. If this is specified, CPP version macros will be generated for this build.",
        ),
    )

    # Tests have an extra fields regarding code coverage.
    if is_test:
        attrs.update({
            "expected_covered_expressions_percentage": attr.int(
                default = -1,
                doc = "The expected percentage of expressions covered by testing.",
            ),
            "expected_uncovered_expression_count": attr.int(
                default = -1,
                doc = "The expected number of expressions which are not covered by testing.",
            ),
            "strict_coverage_analysis": attr.bool(
                default = False,
                doc = "Requires that the coverage metric is matched exactly, even doing better than expected is not allowed.",
            ),
            "coverage_report_format": attr.string(
                default = "text",
                doc = """The format to output the coverage report in. Supported values: "text", "html". Default: "text".
                Report can be seen in the testlog XML file, or by setting --test_output=all when running bazel coverage.
                """,
            ),
            "experimental_coverage_source_patterns": attr.string_list(
                default = ["//..."],
                doc = """The path patterns specifying which targets to analyze for test coverage metrics.

                Wild-card targets such as //... or //:all are allowed. The paths must be relative to the workspace, which means they must start with "//".

                Note, this attribute may leave experimental status depending on the outcome of https://github.com/bazelbuild/bazel/issues/7763.
                """,
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
            "repl": "%{name}@repl",
            "repl_deprecated": "%{name}-repl",
        },
        toolchains = [
            "@rules_haskell//haskell:toolchain",
        ],
        fragments = ["cpp"],
        **kwargs
    )

_haskell_test = _mk_binary_rule(test = True)

_haskell_binary = _mk_binary_rule()

_haskell_library = rule(
    _haskell_library_impl,
    attrs = dict(
        _haskell_common_attrs,
        hidden_modules = attr.string_list(
            doc = "Modules that should be unavailable for import by dependencies.",
        ),
        reexported_modules = attr.label_keyed_string_dict(
            doc = "A dictionary mapping dependencies to module reexports that should be available for import by dependencies.",
        ),
        exports = attr.label_list(
            default = [],
            doc = "A list of other haskell libraries that will be transparently added as a dependency to every downstream rule",
        ),
        linkstatic = attr.bool(
            default = False,
            doc = "Create a static library, not both a static and a shared library.",
        ),
        package_name = attr.string(
            doc = """Library name used in version macro generation. Only used
            if the version attribute is defined, see version attribute
            documentation. Optional, defaults to target name.""",
        ),
        version = attr.string(
            doc = """Library version. Not normally necessary unless to build a library
            originally defined as a Cabal package. If this is specified, CPP version macro will be generated.""",
        ),
    ),
    outputs = {
        "runghc": "%{name}@runghc",
        "repl": "%{name}@repl",
        "repl_deprecated": "%{name}-repl",
    },
    toolchains = [
        "@rules_haskell//haskell:toolchain",
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

def haskell_binary(**kwargs):
    """Build an executable from Haskell source.

    Example:
    ```bzl
    haskell_binary(
    name = "hello",
    srcs = ["Main.hs", "Other.hs"],
    deps = ["//lib:some_lib"]
    )
    ```

    Every `haskell_binary` target also defines an optional REPL target that is
    not built by default, but can be built on request. The name of the REPL
    target is the same as the name of binary with `"@repl"` added at the end.
    For example, the target above also defines `main@repl`.

    You can call the REPL like this (requires Bazel 0.15 or later):

    ```
    $ bazel run //:hello@repl
    ```

    """
    _haskell_worker_wrapper("binary", **kwargs)

def haskell_test(**kwargs):
    """Build a test suite.

    Additionally, it accepts [all common bazel test rule
    fields][bazel-test-attrs]. This allows you to influence things like
    timeout and resource allocation for the test.

    [bazel-test-attrs]: https://docs.bazel.build/versions/master/be/common-definitions.html#common-attributes-tests
    """
    _haskell_worker_wrapper("test", **kwargs)

def haskell_library(**kwargs):
    """Build a library from Haskell source.

    Example:
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

    Every `haskell_library` target also defines an optional REPL target that is
    not built by default, but can be built on request. It works the same way as
    for `haskell_binary`.
    """
    _haskell_worker_wrapper("library", **kwargs)

haskell_import = rule(
    _haskell_import_impl,
    attrs = {
        "id": attr.string(),
        "version": attr.string(),
        "deps": attr.label_list(),
        "static_libraries": attr.label_list(allow_files = [".a"]),
        "shared_libraries": attr.label_list(allow_files = [".dll", ".dylib", ".so"]),
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
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
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
    fragments = ["cpp"],
)
"""Import packages that are prebuilt outside of Bazel.

Example:
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
"""

haskell_doc = _haskell_doc

haskell_doc_aspect = _haskell_doc_aspect

haskell_register_toolchains = _haskell_register_toolchains

haskell_repl = _haskell_repl

haskell_repl_aspect = _haskell_repl_aspect

haskell_toolchain = _haskell_toolchain

ghc_plugin = _ghc_plugin
