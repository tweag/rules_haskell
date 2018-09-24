.. _use-cases:

Common Haskell Build Use Cases
==============================

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

.. _haskell_binary: http://api.haskell.build/haskell/haskell.html#haskell_binary
.. _haskell_library: http://api.haskell.build/haskell/haskell.html#haskell_library

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
