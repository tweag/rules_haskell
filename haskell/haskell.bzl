"""Core Haskell rules"""

load(
    "@io_tweag_rules_haskell//haskell:private/providers.bzl",
    "HaskellPrebuiltPackageInfo",
)
load(
    ":c2hs.bzl",
    _c2hs_library = "c2hs_library",
)
load(
    ":cc.bzl",
    _cc_haskell_import = "cc_haskell_import",
    _haskell_cc_import = "haskell_cc_import",
)
load(
    ":doctest.bzl",
    _haskell_doctest = "haskell_doctest",
    _haskell_doctest_toolchain = "haskell_doctest_toolchain",
)
load(
    ":ghc_bindist.bzl",
    _ghc_bindist = "ghc_bindist",
)
load(
    ":haddock.bzl",
    _haskell_doc = "haskell_doc",
    _haskell_doc_aspect = "haskell_doc_aspect",
)
load(
    ":lint.bzl",
    _haskell_lint = "haskell_lint",
    _haskell_lint_aspect = "haskell_lint_aspect",
)
load(
    ":private/haskell_impl.bzl",
    _haskell_binary_impl = "haskell_binary_impl",
    _haskell_library_impl = "haskell_library_impl",
)

# For re-exports:
load(
    ":protobuf.bzl",
    _haskell_proto_library = "haskell_proto_library",
    _haskell_proto_toolchain = "haskell_proto_toolchain",
)
load(
    ":toolchain.bzl",
    _haskell_toolchain = "haskell_toolchain",
)

_haskell_common_attrs = {
    "src_strip_prefix": attr.string(
        doc = "Directory in which module hierarchy starts.",
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
        doc = "Flags to pass to Haskell compiler.",
    ),
    "prebuilt_dependencies": attr.string_list(
        doc = "Non-Bazel supplied Cabal dependencies (deprecated).",
    ),
    "repl_ghci_args": attr.string_list(
        doc = "Arbitrary extra arguments to pass to GHCi. This extends `compiler_flags` and `repl_ghci_args` from the toolchain",
    ),
    "_ghci_script": attr.label(
        allow_single_file = True,
        default = Label("@io_tweag_rules_haskell//haskell:assets/ghci_script"),
    ),
    "_ghci_repl_wrapper": attr.label(
        allow_single_file = True,
        default = Label("@io_tweag_rules_haskell//haskell:private/ghci_repl_wrapper.sh"),
    ),
    "_ls_modules": attr.label(
        executable = True,
        cfg = "host",
        default = Label("@io_tweag_rules_haskell//haskell:ls_modules"),
    ),
    "_cc_toolchain": attr.label(
        default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
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
    return rule(
        _haskell_binary_impl,
        executable = True,
        attrs = dict(
            _haskell_common_attrs,
            linkstatic = attr.bool(
                default = True,
                doc = "Link dependencies statically wherever possible. Some system libraries may still be linked dynamically, as are libraries for which there is no static library. So the resulting executable will still be dynamically linked, hence only mostly static.",
            ),
            generate_so = attr.bool(
                default = False,
                doc = "Whether to generate also a .so version of executable.",
            ),
            main_function = attr.string(
                default = "Main.main",
                doc = """A function with type `IO _`, either the qualified name of a function from any module or the bare name of a function from a `Main` module. It is also possible to give the qualified name of any module exposing a `main` function.""",
            ),
            main_file = attr.label(
                allow_single_file = [".hs", ".hsc", ".lhs"],
                doc = "File containing `Main` module (deprecated).",
            ),
            version = attr.string(
                doc = "Executable version. If this is specified, CPP version macros will be generated for this build.",
            ),
        ),
        outputs = {
            "repl": "%{name}@repl",
            "repl_deprecated": "%{name}-repl",
        },
        toolchains = [
            "@io_tweag_rules_haskell//haskell:toolchain",
        ],
        **kwargs
    )

haskell_test = _mk_binary_rule(test = True)
"""Build a test suite.

Additionally, it accepts [all common bazel test rule
fields][bazel-test-attrs]. This allows you to influence things like
timeout and resource allocation for the test.

[bazel-test-attrs]: https://docs.bazel.build/versions/master/be/common-definitions.html#common-attributes-tests
"""

haskell_binary = _mk_binary_rule()
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

haskell_library = rule(
    _haskell_library_impl,
    attrs = dict(
        _haskell_common_attrs,
        hidden_modules = attr.string_list(
            doc = "Modules that should be unavailable for import by dependencies.",
        ),
        exports = attr.label_keyed_string_dict(
            doc = "A dictionary mapping dependencies to module reexports that should be available for import by dependencies.",
        ),
        linkstatic = attr.bool(
            default = False,
            doc = "Create a static library, not both a static and a shared library.",
        ),
        version = attr.string(
            doc = """Library version. Not normally necessary unless to build a library
            originally defined as a Cabal package. If this is specified, CPP version macro will be generated.""",
        ),
    ),
    outputs = {
        "repl": "%{name}@repl",
        "repl_deprecated": "%{name}-repl",
    },
    toolchains = [
        "@io_tweag_rules_haskell//haskell:toolchain",
    ],
)
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
      exports = {
          "//hello-sublib:lib": "Lib1 as HelloLib1, Lib2",
      },
  )
  ```

Every `haskell_library` target also defines an optional REPL target that is
not built by default, but can be built on request. It works the same way as
for `haskell_binary`.
"""

def _haskell_import_impl(ctx):
    if ctx.attr.package:
        package = ctx.attr.package
    else:
        package = ctx.label.name
    return [HaskellPrebuiltPackageInfo(package = package)]

haskell_import = rule(
    _haskell_import_impl,
    attrs = dict(
        package = attr.string(doc = "A non-Bazel-supplied GHC package name.  Defaults to the name of the rule."),
    ),
)
"""Import packages that are prebuilt outside of Bazel.

Example:
  ```bzl
  haskell_import(
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
of the compiler toolchain or elsewhere in the environment) available
as targets.

Often, targets of this type will be generated automatically by
frameworks such as Hazel.

"""

haskell_doc = _haskell_doc

haskell_doc_aspect = _haskell_doc_aspect

haskell_lint = _haskell_lint

haskell_lint_aspect = _haskell_lint_aspect

haskell_doctest = _haskell_doctest

haskell_doctest_toolchain = _haskell_doctest_toolchain

haskell_toolchain = _haskell_toolchain

haskell_proto_library = _haskell_proto_library

haskell_proto_toolchain = _haskell_proto_toolchain

ghc_bindist = _ghc_bindist

haskell_cc_import = _haskell_cc_import

cc_haskell_import = _cc_haskell_import

c2hs_library = _c2hs_library
