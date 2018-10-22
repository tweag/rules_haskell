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
