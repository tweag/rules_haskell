.. _use-cases:

Common Haskell Build Use Cases
==============================

Picking a compiler
------------------

Unlike Bazel's native C++ rules, rules_haskell does not auto-detect
a Haskell compiler toolchain from the environment. This is by design.
We require that you declare a compiler to use in your ``WORKSPACE``
file.

There are two common sources for a compiler. One is to use the
official binary distributions from `haskell.org`_. This is done using
the `ghc_bindist`_ rule.

The compiler can also be pulled from Nixpkgs_, a set of package
definitions for the `Nix package manager`_. Pulling the compiler from
Nixpkgs makes the build more hermetic, because the transitive closure
of the compiler and all its dependencies is precisely defined in the
``WORKSPACE`` file. Use `rules_nixpkgs`_ to do so (where ``X.Y.Z``
stands for any recent release)::

  load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

  http_archive(
      name = "io_tweag_rules_nixpkgs",
      strip_prefix = "rules_nixpkgs-X.Y.Z",
      urls = ["https://github.com/tweag/rules_nixpkgs/archive/vX.Y.Z.tar.gz"],
  )

  load(
      "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
      "nixpkgs_git_repository",
      "nixpkgs_package"
  )

  nixpkgs_git_repository(
      name = "nixpkgs",
      revision = "18.09", # Any tag or commit hash
  )

  nixpkgs_package(
      name = "ghc",
      repositories = { "nixpkgs": "@nixpkgs//:default.nix" }
      attribute_path = "haskell.compiler.ghc843", # Any compiler version
      build_file = "@io_tweag_rules_haskell//haskell:ghc.BUILD",
  )

  register_toolchains("//:ghc")

This workspace description specifies which Nixpkgs version to use,
then exposes a Nixpkgs package containing the GHC compiler. The
description assumes that there exists a ``BUILD`` file at the root of
the repository that includes the following::

  haskell_toolchain(
    name = "ghc",
    # Versions here and in WORKSPACE must match.
    version = "8.4.3",
    # Use binaries from @ghc//:bin to define //:ghc toolchain.
    tools = ["@ghc//:bin"],
  )

.. _Bazel+Nix blog post: https://www.tweag.io/posts/2018-03-15-bazel-nix.html
.. _Nix package manager: https://nixos.org/nix
.. _Nixpkgs: https://nixos.org/nixpkgs/manual/
.. _ghc_bindist: http://api.haskell.build/haskell/ghc_bindist.html#ghc_bindist
.. _haskell.org: https://haskell.org
.. _haskell_binary: http://api.haskell.build/haskell/haskell.html#haskell_binary
.. _haskell_library: http://api.haskell.build/haskell/haskell.html#haskell_library
.. _rules_nixpkgs: https://github.com/tweag/rules_nixpkgs

Loading targets in a REPL
-------------------------

Rebuilds are currently not incremental *within* a binary or library
target (rebuilds are incremental across targets of course). Any change
in any source file will trigger a rebuild of all source files listed
in a target. In Bazel, it is conventional to decompose libraries into
small units. In this way, libraries require less work to rebuild.
Still, for interactive development full incrementality and fast
recompilation times are crucial for a good developer experience. We
recommend making all development REPL-driven for fast feedback when
source files change.

Every `haskell_binary`_ and every `haskell_library`_ target has an
optional executable output that can be run to drop you into an
interactive session. If the target's name is ``foo``, then the REPL
output is called ``foo@repl``.

Consider the following binary target::

  haskell_binary(
      name = "hello",
      srcs = ["Main.hs", "Other.hs"],
      deps = ["//lib:some_lib"],
  )

The target above also implicitly defines ``hello@repl``. You can call
the REPL like this (requires Bazel 0.15 or later)::

  $ bazel run //:hello@repl

This works for any ``haskell_binary`` or ``haskell_library`` target.
Modules of all libraries will be loaded in interpreted mode and can be
reloaded using the ``:r`` GHCi command when source files change.

Building code with Hackage dependencies (using Nix)
---------------------------------------------------

Each Haskell library or binary needs a simple build description to
tell Bazel what source files to use and what the dependencies are, if
any. Packages on Hackage don't usually ship with `BUILD.bazel` files.
So if your code depends on them, you either need to write a build
description for each package, generate one (see next section), or
decide not to use Bazel to build packages published on Hackage. This
section documents one way to do the latter.

Nix is a package manager. The set of package definitions is called
Nixpkgs. This repository contains definitions for most actively
maintained Cabal packages published on Hackage. Where these packages
depend on system libraries like zlib, ncurses or libpng, Nixpkgs also
contains package descriptions for those, and declares those as
dependencies of the Cabal packages. Since these definitions already
exist, we can reuse them instead of rewriting these definitions as
build definitions in Bazel. See the `Bazel+Nix blog post`_ for a more
detailed rationale.

To use Nixpkgs in Bazel, we need `rules_nixpkgs`_. See `Picking
a compiler`_ for how to import Nixpkgs rules into your workspace and
how to use a compiler from Nixpkgs. To use Cabal packages from
Nixpkgs, replace the compiler definition with the following::

  nixpkgs_package(
      name = "ghc",
      repositories = { "nixpkgs": "@nixpkgs//:default.nix" },
      nix_file = "//:ghc.nix",
      build_file = "@io_tweag_rules_haskell//haskell:ghc.BUILD",
  )

This definition assumes a ``ghc.nix`` file at the root of the
repository. In this file, you can use the Nix expression language to
construct a compiler with all the packages you depend on in scope::

  with (import <nixpkgs> {});

  haskellPackages.ghcWithPackages (p: with p; [
    containers
    lens
    text
  ])

Each package mentioned in ``ghc.nix`` can then be imported using
`haskell_toolchain_library`_ in ``BUILD`` files.

.. _haskell_toolchain_library: http://api.haskell.build/haskell/haskell.html#haskell_toolchain_library

Building code with Hackage dependencies (using Hazel)
-----------------------------------------------------

.. todo::

   Explain how to use Hazel instead of Nix

Generating API documentation
----------------------------

The `haskell_doc`_ rule can be used to build API documentation for
a given library (using Haddock). Building a target called
``//my/pkg:mylib_docs`` would make the documentation available at
``bazel-bin/my/pkg/mylib_docs/index/index.html``.

Alternatively, you can use the
``@io_tweag_rules_haskell//haskell:haskell.bzl%haskell_doc_aspect``
aspect to ask Bazel from the command-line to build documentation for
any given target (or indeed all targets), like in the following:

.. code-block:: console

  $ bazel build //my/pkg:mylib \
      --aspects @io_tweag_rules_haskell//haskell:haskell.bzl%haskell_doc_aspect

.. _haskell_doc: http://api.haskell.build/haskell/haddock.html#haskell_doc

Linting your code
-----------------

The `haskell_lint`_ rule does not build code but runs the GHC
typechecker on all listed dependencies. Warnings are treated as
errors.

Alternatively, you can directly check a target using

.. code-block:: console

  $ bazel build //my/haskell:target \
      --aspects @io_tweag_rules_haskell//haskell:haskell.bzl%haskell_lint_aspect

.. _haskell_lint: http://api.haskell.build/haskell/lint.html#haskell_lint

Checking code coverage
----------------------

"Code coverage" is the name given to metrics that describe how much source 
code is covered by a given test suite.  A specific code coverage metric 
implemented here is expression coverage, or the number of expressions in 
the source code that are explored when the tests are run.

Haskell's ``ghc`` compiler has built-in support for code coverage analysis, 
through the hpc_ tool. The Haskell rules allow the use of this tool to analyse 
``haskell_library`` coverage by ``haskell_test`` rules. To do so, you have a 
few options. You can add 
``expected_covered_expressions_percentage=<some integer between 0 and 100>`` to
the attributes of a ``haskell_test``, and if the expression coverage percentage
is lower than this amount, the test will fail. Alternatively, you can add
``expected_uncovered_expression_count=<some integer greater or equal to 0>`` to
the attributes of a ``haskell_test``, and instead the test will fail if the
number of uncovered expressions is greater than this amount. Finally, you could
do both at once, and have both of these checks analyzed by the coverage runner.
To see the coverage details of the test suite regardless of if the test passes
or fails, add ``--test_output=all`` as a flag when invoking the test, and there 
will be a report in the test output. You will only see the report if you
required a certain level of expression coverage in the rule attributes.

For example, your BUILD file might look like this: ::

  haskell_library(
    name = "lib",
    srcs = ["Lib.hs"],
    deps = [
        "//tests/hackage:base",
    ],
  )

  haskell_test(
    name = "test",
    srcs = ["Main.hs"],
    deps = [
        ":lib",
        "//tests/hackage:base",
    ],
    expected_covered_expressions_percentage = 80,
    expected_uncovered_expression_count = 10,
  )

And if you ran ``bazel coverage //somepackage:test --test_output=all``, you 
might see a result like this: ::

  INFO: From Testing //somepackage:test:
  ==================== Test output for //somepackage:test:
  Overall report
  100% expressions used (9/9)
  100% boolean coverage (0/0)
      100% guards (0/0)
      100% 'if' conditions (0/0)
      100% qualifiers (0/0)
  100% alternatives used (0/0)
  100% local declarations used (0/0)
  100% top-level declarations used (3/3)
  =============================================================================

Here, the test passes because it actually has 100% expression coverage and 0
uncovered expressions, which is even better than we expected on both counts.

There is an optional ``haskell_test`` attribute called
``strict_coverage_analysis``, which is a boolean that changes the coverage
analysis such that even having better coverage than expected fails the test.
This can be used to enforce that developers must upgrade the expected test
coverage when they improve it. On the other hand, it requires changing the
expected coverage for almost any change.

There a couple of notes regarding the coverage analysis functionality:

- Coverage analysis currently is scoped to all source files and all
  locally-built Haskell dependencies (both direct and transitive) for a given
  test rule.
- Coverage-enabled build and execution for ``haskell_test`` targets may take
  longer than regular. However, this has not effected regular ``run`` /
  ``build`` / ``test`` performance.

.. _hpc: <http://hackage.haskell.org/package/hpc>
