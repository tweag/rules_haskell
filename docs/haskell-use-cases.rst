.. _use-cases:

Common Haskell Build Use Cases
==============================

Starting a new project
----------------------

The fastest way to start a new project is::

  $ curl https://haskell.build/start | sh

The script checks that the version of Bazel installed on your system
is known to be compatible with rules_haskell and creates a new Bazel
workspace in the current working directory with a few dummy build
targets. See the following sections about customizing the workspace.

Making rules_haskell available
------------------------------

First of all, the ``WORKSPACE`` file must specify how to obtain
rules_haskell. To use a released version, do the following::

  load(
      "@bazel_tools//tools/build_defs/repo:http.bzl",
      "http_archive"
  )

  http_archive(
      name = "rules_haskell",
      sha256 = "34742848a8882d94a0437b3b1917dea6f58c82fe5762afe8d249d3a36e51935d",
      strip_prefix = "rules_haskell-0.19",
      url = "https://github.com/tweag/rules_haskell/releases/download/v0.19/rules_haskell-0.19.tar.gz",
  )

Picking a compiler
------------------

Unlike Bazel's native C++ rules, rules_haskell does not auto-detect
a Haskell compiler toolchain from the environment. This is by design.
We require that you declare a compiler to use in your ``WORKSPACE``
file.

There are two common sources for a compiler. One is to use the
official binary distributions from `haskell.org`_. This is done using
the `ghc_bindist`_ rule. You don't normally need to call this rule
directly. You can instead call the following macro, which exposes all
binary distributions for all platforms (Bazel will select one during
toolchain resolution based on the target platform)::

  load(
      "@rules_haskell//haskell:toolchain.bzl",
      "rules_haskell_toolchains",
  )

  rules_haskell_toolchains(
      version = "X.Y.Z", # Any GHC version
  )


The compiler can also be pulled from Nixpkgs_, a set of package
definitions for the `Nix package manager`_. Pulling the compiler from
Nixpkgs makes the build more hermetic, because the transitive closure
of the compiler and all its dependencies is precisely defined in the
``WORKSPACE`` file::

  load(
      "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
      "nixpkgs_git_repository",
  )

  nixpkgs_git_repository(
      name = "nixpkgs",
      revision = "19.03", # Any tag or commit hash
  )

  load(
      "@rules_haskell//haskell:nixpkgs.bzl",
      "haskell_register_ghc_nixpkgs",
  )

  haskell_register_ghc_nixpkgs(
      version = "X.Y.Z", # Any GHC version
      attribute_path = "ghc", # The Nix attribute path to the compiler.
      repositories = {"nixpkgs": "@nixpkgs"},
  )

This workspace description specifies which Nixpkgs version to use,
then invokes a workspace macro that exposes a Nixpkgs package
containing the GHC compiler and registers this compiler as a toolchain
usable by Bazel's Haskell rules.

You can register as many toolchains as you like. Nixpkgs toolchains do
not conflict with binary distributions. For Bazel to select the
Nixpkgs toolchain during `toolchain resolution`_, set the platform
accordingly. For example, you can have the following in your
``.bazelrc`` file at the root of your project::

  build --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host

.. _Bazel+Nix blog post: https://www.tweag.io/posts/2018-03-15-bazel-nix.html
.. _Nix package manager: https://nixos.org/nix
.. _Nixpkgs: https://nixos.org/nixpkgs/manual/
.. _ghc_bindist: https://api.haskell.build/haskell/ghc_bindist.html#ghc_bindist
.. _haskell.org: https://haskell.org
.. _haskell_binary: https://api.haskell.build/haskell/defs.html#haskell_binary
.. _haskell_library: https://api.haskell.build/haskell/defs.html#haskell_library
.. _rules_nixpkgs: https://github.com/tweag/rules_nixpkgs
.. _toolchain resolution: https://docs.bazel.build/versions/master/toolchains.html#toolchain-resolution

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

Configuring IDE integration with ghcide
---------------------------------------

rules_haskell has preliminary support for IDE integration using `ghcide`_. The
ghcide project provides IDE features for Haskell projects through the Language
Server Protocol. To set this up you can define a `haskell_repl`_ target that
will collect the required compiler flags for your Haskell targets and pass them
to `hie-bios`_ which will then forward them to ghcide.

Let's set this up for the following example project::

  haskell_toolchain_library(
      name = "base",
  )

  haskell_library(
      name = "library-a",
      srcs = ["Lib/A.hs"],
      deps = [":base"],
  )

  haskell_library(
      name = "library-b",
      srcs = ["Lib/B.hs"],
      deps = [":base"],
  )

  haskell_binary(
      name = "binary",
      srcs = ["Main.hs"],
      deps = [
          ":base",
          ":library-a",
          ":library-b",
      ],
  )

We want to configure ghcide to provide IDE integration for all these three
targets. Start by defining a ``haskell_repl`` target as follows::

  haskell_repl(
    name = "hie-bios",
    collect_data = False,
    deps = [
      ":binary",
      # ":library-a",
      # ":library-b",
    ],
  )

Note, that ``library-a`` and ``library-b`` do not have to be listed explicitly.
By default haskell_repl will include all transitive dependencies that are not
external dependencies. Refer to the API documentation of `haskell_repl`_ for
details.

We also disable building runtime dependencies using ``collect_data = False`` as
they are not required for an IDE session.

You can test if this provides the expected compiler flags by running
the following Bazel command:

  bazel run //:hie-bios@bios

Next, we need to hook this up to `hie-bios`_ using the `bios cradle`_. To that
end, define a small shell script named ``.hie-bios`` that looks as follows::

  #!/usr/bin/env bash
  set -euo pipefail
  bazel run //:hie-bios@bios
  # Make warnings non-fatal
  echo -Wwarn >>"$HIE_BIOS_OUTPUT"

Then configure `hie-bios`_ to use this script in the bios cradle with the
following ``hie.yaml`` file::

  cradle:
    bios:
      program: ".hie-bios"

Now the hie-bios cradle is ready to use. The last step is to install ghcide.
Unfortunately, ghcide has to be compiled with the exact same GHC that you're
using to build your project. The easiest way to do this is in this context is
to build it with Bazel as part of your rules_haskell project.

First, define a custom stack snapshot that provides the package versions that
ghcide requires based on `ghcide's stack.yaml`_ file. Let's call it
``ghcide-stack-snapshot.yaml``. Copy the ``resolver`` field and turn the
``extra-deps`` field into a ``packages`` field. Then add another entry to
``packages`` for the ghcide library itself::

  # Taken from ghcide's stack.yaml
  resolver: nightly-2019-09-21
  packages:
    # Taken from the extra-deps field.
    - haskell-lsp-0.21.0.0
    - haskell-lsp-types-0.21.0.0
    - lsp-test-0.10.2.0
    - hie-bios-0.4.0
    - fuzzy-0.1.0.0
    - regex-pcre-builtin-0.95.1.1.8.43
    - regex-base-0.94.0.0
    - regex-tdfa-1.3.1.0
    - shake-0.18.5
    - parser-combinators-1.2.1
    - haddock-library-1.8.0
    - tasty-rerun-1.1.17
    - ghc-check-0.1.0.3
    # Point to the ghcide revision that you would like to use.
    - github: digital-asset/ghcide
      commit: "39605333c34039241768a1809024c739df3fb2bd"
      sha256: "47cca96a6e5031b3872233d5b9ca14d45f9089da3d45a068e1b587989fec4364"

Then define a dedicated ``stack_snapshot`` for ghcide in your ``WORKSPACE``
file. The ``ghcide`` package has a library and an executable component which we
need to declare using the ``components`` attribute::

  stack_snapshot(
      name = "ghcide",
      # The rules_haskell example project shows how to import libz.
      # https://github.com/tweag/rules_haskell/blob/123e3817156f9135dfa44dcb5a796c424df1f436/examples/WORKSPACE#L42-L63
      extra_deps = {"zlib": ["@zlib.hs"]},
      haddock = False,
      local_snapshot = "//:ghcide-stack-snapshot.yaml",
      packages = ["ghcide"],
      components = {"ghcide": ["lib", "exe"]},
  )

This will make the ``ghcide`` executable available under the Bazel label
``@ghcide-exe//ghcide``. You can test if this worked by building and executing
ghcide as follows::

  bazel build @ghcide-exe//ghcide
  bazel-bin/external/ghcide/ghcide-0.1.0/_install/bin/ghcide

Write a small shell script to make it easy to invoke ghcide from your editor::

  #!/usr/bin/env bash
  set -euo pipefail
  bazel build @ghcide-exe//ghcide
  bazel-bin/external/ghcide/ghcide-0.1.0/_install/bin/ghcide "$@"

And, the last step, configure your editor to use ghcide. The upstream
documentation provides `ghcide setup instructions`_ for a few popular editors.
Be sure to configure your editor to invoke the above wrapper script instead of
another instance of `ghcide`. Also note, that if you are using Nix, then you
may need to invoke ghcide within a ``nix-shell``.

.. _ghcide: https://github.com/digital-asset/ghcide
.. _haskell_repl: https://api.haskell.build/haskell/defs.html#haskell_repl
.. _hie-bios: https://github.com/mpickering/hie-bios
.. _bios cradle: https://github.com/mpickering/hie-bios#bios
.. _ghcide's stack.yaml: https://github.com/digital-asset/ghcide/blob/39605333c34039241768a1809024c739df3fb2bd/stack.yaml
.. _ghcide setup instructions: https://github.com/digital-asset/ghcide#using-with-vs-code

Building Cabal packages
-----------------------

If you depend on third-party code hosted on Hackage_, these will have
a build script that uses the Cabal_ framework. Bazel can build these
with the `haskell_cabal_library`_ and `haskell_cabal_binary`_ rules.
However, you seldom want to use them directly. Cabal packages
typically have many dependencies, which themselves have dependencies
and so on. It is tedious to describe all of these dependencies to
Bazel by hand. You can use the `stack_snapshot`_ workspace rule
as described below to download the source of all necessary dependencies from
Hackage, and extract a dependency graph from a Stackage_ snapshot.

These rules are meant only to interoperate with third-party code. For
code under your direct control, prefer using one of the core Haskell
rules, which have more features, are more efficient and more
customizable.

Importing a Stackage snapshot
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The `stack_snapshot`_ workspace rule interfaces with the Stack tool to resolve
package versions and dependencies based on a given Stackage snapshot. It also
downloads the packages sources and generates Bazel build definitions for the
individual Cabal packages.

This is how you import the Stackage LTS 20.3 snapshot ::

  stack_snapshot(
      name = "stackage",
      snapshot = "lts-20.3",
      packages = [
          "base",
          "optparse-applicative",
      ],
  )

This will generate the labels ``@stackage//:base``, and
``@stackage//:optparse-applicative``, which you can use in the ``deps``
attribute of your Haskell targets. Note that ``base`` is a core package and its
version is determined by the GHC toolchain and not the Stackage snapshot.

Use the ``local_snapshot`` attribute to refer to a `custom Stack snapshot`_.

Pinning
^^^^^^^

The ``stack_snapshot`` rule invokes ``stack`` for version and dependency
resolution.  By default this will happen on every fetch of the `external
repository`_. This may require arbitrary network access, which can slow down
the build. It may also lead to reproducibility issues, for example if a new
revision of a Hackage dependency is published. Finally, ``stack`` downloading
packages is opaque to Bazel and therefore not eligible for `repository caching`_.

You can enable pinning to avoid these issues. In this case ``stack`` will be
called only once to perform dependency resolution and the results will be
written to a lock file. Future fetches will only read from that lock file and
download packages in a way that is eligible for Bazel repository caching.

1. Generate a lock file by running ``bazel run @stackage-unpinned//:pin``.
2. Set the ``stack_snapshot_json`` attribute. ::

     stack_snapshot(
         ...
         stack_snapshot_json = "//:stackage_snapshot.json",
     )

Repeat step 1 when you change the ``stack_snapshot`` definition, e.g. the
Stackage snapshot or the list of packages.

Version overrides or Hackage dependencies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can also depend on Hackage packages that are not part of a Stackage
snapshot, or override the version of a package, by specifying the version in
the ``packages`` attribute. ::

  stack_snapshot(
      ...
      packages = [
          ...
          "optparse-helper-0.2.1.1",
      ],
  )

Non-Haskell dependencies
^^^^^^^^^^^^^^^^^^^^^^^^

Some Hackage packages depend on C libraries. Bazel builds should be hermetic,
therefore, such library dependencies should be managed by Bazel and declared
explicitly. ::

  stack_snapshot(
      ...
      packages = [
          ...
          "zlib",
      ],
      extra_deps = {
          "zlib": ["@zlib-deps//:libz"],
      },
  )

This declares that the Stackage package ``zlib`` has an additional dependency
``@zlib-deps//:libz``. The C library ``libz`` could be imported using
``rules_nixpkgs``, or fetched and built by Bazel as follows. ::

  http_archive(
      name = "zlib-deps",
      build_file_content = """
  load("@rules_cc//cc:defs.bzl", "cc_library")
  cc_library(
      name = "libz",
      # The indirection enforces the library name `libz.so`,
      # otherwise Cabal won't find it.
      srcs = [":z"],
      hdrs = glob(["*.h"]),
      includes = ["."],
      visibility = ["//visibility:public"],
  )
  cc_library(name = "z", srcs = glob(["*.c"]), hdrs = glob(["*.h"]))
  """,
      sha256 = "b5b06d60ce49c8ba700e0ba517fa07de80b5d4628a037f4be8ad16955be7a7c0",
      strip_prefix = "zlib-1.3",
      urls = ["https://github.com/madler/zlib/archive/v1.3.tar.gz"],
  )

Vendoring packages
^^^^^^^^^^^^^^^^^^

You can inject a vendored or patched version of a package into the dependency
graph generated by ``stack_snapshot``. For example, if you have a custom
version of the ``hashable`` package in your repository under the label
``//third-party/hashable``, then you can inject it into a ``stack_snapshot`` as
follows. ::

  workspace(name = "workspace-name")

  stack_snapshot(
      ...
      packages = [
          ...
          "unordered-containers",
      ],
      vendored_packages = {
          "hashable": "@workspace-name//third-party/hashable",
      },
  )

In this case the package ``unordered-containers`` will be linked against your
vendored version of ``hashable`` instead of the version defined by the original
Stackage snapshot.

Note that ``stack_snapshot`` still needs a Cabal file of vendored packages for
version and dependency resolution. In the above example the Cabal file should
be a static file under the label ``//third-party/hashable:hashable.cabal``.

The vendored package does not have to be local to your workspace. Instead, it
could be an external repository imported by a rule such as ``http_archive``,
``local_repository``, or ``new_local_repository``. A common use-case is to
patch version bounds as described below.

Patching packages
^^^^^^^^^^^^^^^^^

The ``vendored_packages`` attribute can be used to inject a patched version of
a Hackage packages, for example one with patched Cabal version bounds. ::

  stack_snapshot(
      ...
      vendored_packages = {
          "split": "@split//:split",
      },
  )

  http_archive(
      name = "split",
      build_file_content = """
  load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
  load("@stackage//:packages.bzl", "packages")
  haskell_cabal_library(
      name = "split",
      version = packages["split"].version,
      srcs = glob(["**"]),
      deps = packages["split"].deps,
      visibility = ["//visibility:public"],
  )
      """,
      patch_args = ["-p1"],
      patches = ["@rules_haskell_examples//:split.patch"],
      sha256 = "1dcd674f7c5f276f33300f5fd59e49d1ac6fc92ae949fd06a0f6d3e9d9ac1413",
      strip_prefix = "split-0.2.3.3",
      urls = ["http://hackage.haskell.org/package/split-0.2.3.3/split-0.2.3.3.tar.gz"],
  )

The ``stack_snapshot`` rule emits metadata determined during dependency
resolution into the file ``packages.bzl``. In the above example this file is
used to avoid manually repeating the version and the list of dependencies of
the ``split`` package, which is already defined in its Cabal file.

.. _Cabal: https://haskell.org/cabal
.. _Hackage: https://hackage.haskell.org
.. _Stackage: https://stackage.org
.. _haskell_cabal_library: https://api.haskell.build/haskell/cabal.html#haskell_cabal_library
.. _haskell_cabal_binary: https://api.haskell.build/haskell/cabal.html#haskell_cabal_binary
.. _stack_snapshot: https://api.haskell.build/haskell/cabal.html#stack_snapshot
.. _custom Stack snapshot: https://docs.haskellstack.org/en/stable/pantry/#snapshots
.. _external repository: https://docs.bazel.build/versions/master/external.html
.. _repository caching: https://docs.bazel.build/versions/master/guide.html#the-repository-cache

Building Cabal packages (using Nix)
-----------------------------------

An alternative to using Bazel to build Cabal packages (like in the
previous section) is to leave this to Nix.

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

  haskell_register_ghc_nixpkgs(
      version = "X.Y.Z", # Any GHC version
      nix_file = "//:ghc.nix",
      build_file = "@rules_haskell//haskell:ghc.BUILD",
      repositories = { "nixpkgs": "@nixpkgs" },
  )

This definition assumes a ``ghc.nix`` file at the root of the
repository. In this file, you can use the Nix expression language to
construct a compiler with all the packages you depend on in scope::

  with (import <nixpkgs> { config = {}; overlays = []; });

  haskellPackages.ghcWithPackages (p: with p; [
    containers
    lens
    text
  ])

Each package mentioned in ``ghc.nix`` can then be imported using
`haskell_toolchain_library`_ in ``BUILD`` files.

.. _haskell_toolchain_library: https://api.haskell.build/haskell/defs.html#haskell_toolchain_library

Building incrementally
----------------------

By default, Haskell rules that build libraries or binaries with multiple
modules will rebuild all of their modules after any change. This is because
``rules_haskell`` doesn't analyze the source code to discover dependencies
between the modules. Instead, it pessimistically assumes that all the modules
depend on every other.

The user, however, can describe the dependencies between the modules,
and avoid unnecessary recompilation by using the ``haskell_module``
rule.::

  load("@rules_haskell//haskell:defs.bzl", "haskell_library")
  load("@rules_haskell//haskell/experimental:defs.bzl", "haskell_module")

  haskell_module(
      name = "LibMod1",
      src = "src/LibMod1.hs",
      src_strip_prefix = "src",
      deps = [":LibMod2"],
      # set to True if TemplateHaskell is needed
      enable_th = True,
  )

  haskell_module(
      name = "LibMod2",
      src = "src/LibMod2.hs",
      src_strip_prefix = "src",
  )

  haskell_library(
      name = "lib",
      # Must choose either one of srcs or modules
      # srcs = ...,
      modules = [
          "LibMod1",
          "LibMod2",
      ],
      deps = [
          "//:base",
          "//:template-haskell",
      ],
  )

Instead of using the ``srcs`` attribute of ``haskell_library``, each
source file gets a ``haskell_module`` rule that is then referenced in
the ``modules`` attribute. When a module depends on another module of
the same library, the dependency can be expressed in the ``deps``
attribute of the ``haskell_module`` rule.

Now, when ``LibMod1.hs`` changes, ``rules_haskell`` can update the
``lib`` target without rebuilding ``LibMod2.hs``, since ``:LibMod2``
doesn't depend on ``:LibMod1``. ``rules_haskell`` can't detect
redundant dependencies, but it will produce an error in sandboxed
builds if ``:LibMod2`` needs a dependency that hasn't been declared.

Dependencies of ``haskell_module`` come in three flavors. Firstly,
required modules can be listed in the ``deps`` attribute. Secondly,
other libraries are dependencies of the module if they appear listed
in the ``deps`` attribute of the enclosing library (that would be
``:lib`` in our example). Finally, the ``haskell_module`` rule can also
depend on specific modules from other libraries via the
``cross_library_deps`` attribute.::

  haskell_module(
      name = "Lib2Mod1",
      src = "src/LibMod1.hs",
      src_strip_prefix = "src",
      deps = [":Lib2Mod2"],
      # Any modules listed here must come from libraries
      # listed in the narrowed_deps of :lib2
      cross_library_deps = [":LibMod2"],
  )

  haskell_module(
      name = "Lib2Mod2",
      src = "src/LibMod2.hs",
      src_strip_prefix = "src",
  )

  haskell_library(
      name = "lib2",
      modules = [
          "Lib2Mod1",
          "Lib2Mod2",
      ],
      deps = [
          "//:base",
          "//:template-haskell",
      ],
      narrowed_deps = [":lib"],
  )

``cross_library_deps`` allows to express dependencies on specific modules
(like ``:LibMod2``), without the build having to depend on other modules
coming from the same library (like ``:LibMod1``). The alternative would be
to add ``:lib`` to the ``deps`` attribute of ``:lib2``, but this would
cause builds of ``:Lib2Mod1`` and ``:Lib2Mod2`` to depend on all of the
modules of ``:lib``, as in the following snippet.::

  haskell_module(
      name = "Lib2Mod1",
      src = "src/LibMod1.hs",
      src_strip_prefix = "src",
      deps = [":Lib2Mod2"],
  )

  haskell_library(
      name = "lib2",
      modules = [
          "Lib2Mod1",
          "Lib2Mod2",
      ],
      deps = [
          ":lib",
          "//:base",
          "//:template-haskell",
      ],
  )

In order to avoid manually keeping the build configuration in sync with
the graph of module imports, there is the `gazelle_haskell_modules`_ tool
which analyzes the source code and updates the ``haskell_module`` rules.

.. _gazelle_haskell_modules: https://github.com/tweag/gazelle_haskell_modules

Generating API documentation
----------------------------

The `haskell_doc`_ rule can be used to build API documentation for
a given library (using Haddock). Building a target called
``//my/pkg:mylib_docs`` would make the documentation available at
``bazel-bin/my/pkg/mylib_docs/index/index.html``.

Alternatively, you can use the
``@rules_haskell//haskell:defs.bzl%haskell_doc_aspect``
aspect to ask Bazel from the command-line to build documentation for
any given target (or indeed all targets), like in the following:

.. code-block:: console

  $ bazel build //my/pkg:mylib \
      --aspects @rules_haskell//haskell:defs.bzl%haskell_doc_aspect

.. _haskell_doc: https://api.haskell.build/haskell/defs.html#haskell_doc

Linting your code
-----------------

There is currently no dedicated rule for linting Haskell code. You can
apply warning flags using the ``compiler_flags`` attribute, for example ::

  haskell_library(
      ...
      ghcopts = [
          "-Werror",
          "-Wall",
          "-Wcompat",
          "-Wincomplete-record-updates",
          "-Wincomplete-uni-patterns",
          "-Wredundant-constraints",
          "-Wnoncanonical-monad-instances",
      ],
      ghci_repl_flags = ["-Wwarn"],
  )

For larger projects it can make sense to define a custom macro that
applies such common flags by default. ::

  common_ghcopts = [ ... ]

  def my_haskell_library(name, ghcopts = [], ...):
      haskell_library(
          name = name,
          ghcopts = common_ghcopts + ghcopts,
          ...
      )

There is currently no builtin support for invoking ``hlint``. However, you
can invoke ``hlint`` in a CI step outside of Bazel. Refer to the `hlint
documentation`_ for further details.

.. _hlint documentation: https://github.com/ndmitchell/hlint#readme

Refer to the `rules_haskell issue tracker`__ for a discussion around
adding an ``hlint`` rule.

.. _hlint issue: https://github.com/tweag/rules_haskell/issues/1140

__ `hlint issue`_

Using conditional compilation
-----------------------------

If all downstream users of a library live in the same repository (as
is typically the case in the `monorepo`_ pattern), then conditional
compilation of any part of the library is typically needed only in
limited circumstances, like cross-platform support. Supporting
multiple versions of upstream dependencies using conditional
compilation is not normally required, because a single set of versions
of all dependencies is known *a priori*. For this reason, compiler
supplied `version macros`_ are disabled by default. Only libraries
with a `version attribute`_ have version macros available during
compilation, and only for those dependencies that themselves have
a version number (this includes Cabal libraries).

Bazel also has support for conditional compilation via the `select
construct`_, which can be used to conditionally include source files
in rule inputs (e.g. different source files for different platforms).

.. _monorepo: https://en.wikipedia.org/wiki/Monorepo
.. _Version macros: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/phases.html#standard-cpp-macros
.. _version attribute: https://api.haskell.build/haskell/defs.html#haskell_library.version
.. _select construct: https://docs.bazel.build/versions/master/configurable-attributes.html

Using source code pre-processors
--------------------------------

GHC allows any number of pre-processors to run before parsing a file.
These pre-processors can be specfied in compiler flags on the
command-line or in pragmas in the source files. For example,
`hspec-discover`_ is a pre-processor. To use it, it must be
a `tools` dependency. You can then use a CPP macro to avoid hardcoding
the location of the tool in source code pragmas. Example: ::

  haskell_test(
      name = "tests",
      srcs = ["Main.hs", "Spec.hs"],
      ghcopts = ["-DHSPEC_DISCOVER=$(location @stackage-exe//hspec-discover)"],
      tools = ["@stackage-exe//hspec-discover"],
      deps = ["@stackage//:base"],
  )

Where ``Spec.hs`` reads: ::

  {-# LANGUAGE CPP #-}
  {-# OPTIONS_GHC -F -pgmF HSPEC_DISCOVER #-}

.. _hspec-discover: https://hackage.haskell.org/package/hspec-discover

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

.. _hpc: https://hackage.haskell.org/package/hpc

Profiling
---------

Exclusive profiling mode is activated by setting the `compilation mode`_
to ``dbg``. In which case, only the profiling libraries and binaries are
compiled (instead of both profiling and non-profiling). In profiling
mode, the toolchain libraries only carry their static archives, as no
shared libraries are provided. (Tests that strictly require shared
objects are disabled in profiling mode.)

.. _compilation mode: https://docs.bazel.build/versions/main/user-manual.html#flag--compilation_mode

Persistent Worker Mode (experimental)
-------------------------------------

Bazel supports the special `persistent worker mode`_ when, instead of calling the compiler
from scratch to build every target separately, it spawns a resident process for this purpose
and sends all compilation requests to it in the client-server fashion. This worker strategy
may improve compilation times. We implemented a worker for GHC using GHC API.

.. _persistent worker mode: https://blog.bazel.build/2015/12/10/java-workers.html

To activate the persistent worker mode in ``rules_haskell`` the user adds a couple of lines
in the ``WORKSPACE`` file to load worker's dependencies: ::

  load("//tools:repositories.bzl", "rules_haskell_worker_dependencies")
  rules_haskell_worker_dependencies()

Then, the user will add ``--define use_worker=True`` in the command line when calling
``bazel build`` or ``bazel test``.

It is worth noting that Bazel's worker strategy is not sandboxed by default. This may
confuse our worker relatively easily. Therefore, it is recommended to supply
``--worker_sandboxing`` to ``bazel build`` -- possibly, via your ``.bazelrc.local`` file.

Building fully-statically-linked binaries
-----------------------------------------

Fully-statically linked binaries have no runtime linkage dependencies and are
thus typically more portable and easier to package (e.g. in containers) than
their dynamically-linked counterparts. The trade-off is that
fully-statically-linked binaries can be larger than dynamically-linked binaries,
due to the fact that all symbols must be bundled into a single output.
``rules_haskell`` has support for building fully-statically-linked binaries
using Nix-provisioned GHC toolchains and the ``static_runtime`` and
``fully_static_link`` attributes of the ``haskell_register_ghc_nixpkgs`` macro::

  load(
      "@rules_haskell//haskell:nixpkgs.bzl",
      "haskell_register_ghc_nixpkgs",
  )

  haskell_register_ghc_nixpkgs(
      version = "X.Y.Z",
      attribute_path = "staticHaskell.ghc",
      repositories = {"nixpkgs": "@nixpkgs"},
      static_runtime = True,
      fully_static_link = True,
  )

Note that the ``attribute_path`` must refer to a GHC derivation capable of
building fully-statically-linked binaries. Often this will require you to
customise a GHC derivation in your Nix package set. If you are unfamiliar with
Nix, one way to add such a custom package to an existing set is with an
*overlay*.  Detailed documentation on overlays is available at
https://nixos.wiki/wiki/Overlays, but for the purposes of this documentation,
it's enough to know that overlays are essentially functions which accept package
sets (conventionally called ``super``) and produce new package sets. We can
write an overlay that modifies the ``ghc`` derivation in its argument to add
flags that allow it to produce fully-statically-linked binaries as follows::

  let
    # Pick a version of Nixpkgs that we will base our package set on (apply an
    # overlay to).
    baseCommit = "..."; # Pick a Nixpkgs version to pin to.
    baseSha = "..."; # The SHA of the above version.

    baseNixpkgs = builtins.fetchTarball {
      name = "nixos-nixpkgs";
      url = "https://github.com/NixOS/nixpkgs/archive/${baseCommit}.tar.gz";
      sha256 = baseSha;
    };

    # Our overlay. We add a `staticHaskell.ghc` path matching that specified in
    # the haskell_register_ghc_nixpkgs rule above which overrides the `ghc`
    # derivation provided in the base set (`super.ghc`) with some necessary
    # arguments.
    overlay = self: super: {
      staticHaskell = {
        ghc = (super.ghc.override {
          enableRelocatedStaticLibs = true;
          enableShared = false;
        }).overrideAttrs (oldAttrs: {
          preConfigure = ''
            ${oldAttrs.preConfigure or ""}
            echo "GhcLibHcOpts += -fPIC -fexternal-dynamic-refs" >> mk/build.mk
            echo "GhcRtsHcOpts += -fPIC -fexternal-dynamic-refs" >> mk/build.mk
          '';
        });
      };
    };

  in
    args@{ overlays ? [], ... }:
      import baseNixpkgs (args // {
        overlays = [overlay] ++ overlays;
      })

In this example we use the ``override`` and ``overrideAttrs`` functions to
produce a GHC derivation suitable for our needs. Ideally,
``enableRelocatedStaticLibs`` and ``enableShared`` should be enough, but
upstream Nixpkgs does not at present reliably pass ``-fexternal-dynamic-refs``
when ``-fPIC`` is passed, which is required to generate fully-statically-linked
executables.

You may wish to base your GHC derivation on one which uses Musl, a C library
designed for static linking (unlike glibc, which can cause issues when linked
statically). `static-haskell-nix`_ is an example of a project which provides
such a GHC derivation and can be used like so::

  let
    baseCommit = "..."; # Pick a Nixpkgs version to pin to.
    baseSha = "..."; # The SHA of the above version.

    staticHaskellNixCommit = "..."; Pick a static-haskell-nix version to pin to.

    baseNixpkgs = builtins.fetchTarball {
      name = "nixos-nixpkgs";
      url = "https://github.com/NixOS/nixpkgs/archive/${baseCommit}.tar.gz";
      sha256 = baseSha;
    };

    staticHaskellNixpkgs = builtins.fetchTarball
      "https://github.com/nh2/static-haskell-nix/archive/${staticHaskellNixCommit}.tar.gz";

    # The `static-haskell-nix` repository contains several entry points for e.g.
    # setting up a project in which Nix is used solely as the build/package
    # management tool. We are only interested in the set of packages that underpin
    # these entry points, which are exposed in the `survey` directory's
    # `approachPkgs` property.
    staticHaskellPkgs = (
      import (staticHaskellNixpkgs + "/survey/default.nix") {}
    ).approachPkgs;

    overlay = self: super: {
      staticHaskell = staticHaskellPkgs.extend (selfSH: superSH: {
        ghc = (superSH.ghc.override {
          enableRelocatedStaticLibs = true;
          enableShared = false;
        }).overrideAttrs (oldAttrs: {
          preConfigure = ''
            ${oldAttrs.preConfigure or ""}
            echo "GhcLibHcOpts += -fPIC -fexternal-dynamic-refs" >> mk/build.mk
            echo "GhcRtsHcOpts += -fPIC -fexternal-dynamic-refs" >> mk/build.mk
          '';
        });
      });
    };

  in
    args@{ overlays ? [], ... }:
      import baseNixpkgs (args // {
        overlays = [overlay] ++ overlays;
      })

If you adopt a Musl-based GHC you should also take care to ensure that the C
toolchain used by ``rules_haskell`` also uses Musl; you can do this using the
``nixpkgs_cc_configure`` rule from ``rules_nixpkgs`` and providing a Nix
expression that supplies appropriate ``cc`` and ``binutils`` derivations::

  nixpkgs_cc_configure(
      repository = "@nixpkgs",

      # The `staticHaskell` attribute in the previous example exposes the
      # Musl-backed `cc` and `binutils` derivations already, so it's just a
      # matter of exposing them to nixpkgs_cc_configure.
      nix_file_content = """
        with import <nixpkgs> { config = {}; overlays = []; }; buildEnv {
          name = "bazel-cc-toolchain";
          paths = [ staticHaskell.stdenv.cc staticHaskell.binutils ];
        }
      """,
  )

With the toolchain taken care of, you can then create fully-statically-linked
binaries by enabling the ``fully_static_link`` feature flag, e.g. in ``haskell_binary``::

  haskell_binary(
      name = ...,
      srcs = [
          ...,
      ],
      ...,
      features = [
          "fully_static_link",
      ],
  )

Note, feature flags can be configured `per target`_, `per package`_, or
globally on the `command line`_.

.. _static-haskell-nix: https://github.com/nh2/static-haskell-nix
.. _per target: https://docs.bazel.build/versions/master/be/common-definitions.html#common.features
.. _per package: https://docs.bazel.build/versions/master/be/functions.html#package.features
.. _command line: https://docs.bazel.build/versions/master/command-line-reference.html#flag--features

Containerization with rules_oci
----------------------------------

Making use of both ``rules_oci`` and ``rules_nixpkgs``, it's possible to containerize
``rules_haskell`` ``haskell_binary`` build targets for deployment. In a nutshell, first we must use
``rules_nixpkgs`` to build a ``dockerTools.buildLayeredImage`` target with the basic library dependencies
required to run a typical Haskell binary. Thereafter, we can use ``rules_oci`` to use this as
a base image upon which we can layer a Bazel built Haskell binary.

Step one is to ensure you have all the necessary ``rules_oci`` paraphernalia loaded in your ``WORKSPACE``
file: ::

  http_archive(
      name = "rules_oci",
      sha256 = "4a276e9566c03491649eef63f27c2816cc222f41ccdebd97d2c5159e84917c3b",
      strip_prefix = "rules_oci-1.7.4",
      url = "https://github.com/bazel-contrib/rules_oci/releases/download/v1.7.4/rules_oci-v1.7.4.tar.gz",
  )

  load("@rules_oci//oci:dependencies.bzl", "rules_oci_dependencies")

  rules_oci_dependencies()

  load("@rules_oci//oci:repositories.bzl", "LATEST_CRANE_VERSION", "oci_register_toolchains")

  oci_register_toolchains(
      name = "oci",
      crane_version = LATEST_CRANE_VERSION,
  )

Then we're ready to specify a base image built using the ``rules_nixpkgs`` ``nixpkgs_package`` rule for ``rules_oci`` to layer its products on top of ::

  nixpkgs_package(
      name = "haskell-base-image",
      repository = "//nixpkgs:default.nix",
      # See below for how to define this
      nix_file = "//nixpkgs:haskellBaseImageDocker.nix",
      build_file_content = """
  package(default_visibility = [ "//visibility:public" ])
  exports_files(["image"])
      """,
  )

Step two requires that we specify our nixpkgs/haskellBaseImageDocker.nix file as follows ::

  # nixpkgs is provisioned by rules_nixpkgs for us which we set to be ./default.nix
  with import <nixpkgs> { system = "x86_64-linux"; };

  # Build the base image.
  # The output of this derivation will be a Docker format archive in the same format as
  # the output of `docker save` that we can feed to
  # [oci_image](https://github.com/bazel-contrib/rules_oci/blob/main/docs/image.md#oci_image)
  # as a base image.
  let
    haskellBase = dockerTools.buildLayeredImage {
      name = "haskell-base-image-unwrapped";
      created = "now";
      contents = [ glibc libffi gmp zlib iana-etc cacert ]; # Here we can specify nix-provisioned libraries our haskell_binary products may need at runtime
    };
    # rules_nixpkgs require the nix output to be a directory,
    # so we create one in which we put the image we've just created
  in runCommand "haskell-base-image" { } ''
    mkdir -p $out
    gunzip -c ${haskellBase} > $out/image
  ''

Step three pulls all this together in a build file to actually assemble our final container image. In a BUILD.bazel file, we'll need the following ::

  load("@rules_oci//oci:defs.bzl", "oci_image", "oci_push")
  load("@rules_pkg//pkg:tar.bzl", "pkg_tar")

  haskell_binary(
      name = "my_binary",
      srcs = ["Main.hs"],
      ghcopts = [
          "-O2",
          "-threaded",
          "-rtsopts",
          "-with-rtsopts=-N",
      ],
      deps = [
          ":my_haskell_library_dep", # for example...
          # ...
      ],
  )

  pkg_tar(
      name = "my_binary_tar",
      srcs = [":my_binary"],
  )

  oci_image(
      name = "my_binary_image",
      base = "@haskell-base-image//image",
      tars = [":pkg_tar"],
      exposed_ports = [ "8000/tcp" ],
      entrypoint = ["/my_binary"],
  )

And you may want to use ``rules_oci`` to push your container image as follows ::

  oci_push(
      name = "my_binary_push",
      image = ":my_binary_image",
      # For example using a GCP GCR repository
      repository = "gcr.io/$project-name-here/$my_binary_image_label",
      remote_tags = ["{BUILD_USER}"],
  )

*n.b.* Due to the `current inability`_ of Nix to be used on macOS (darwin) for building Docker images, it's currently
not possible to build Docker images for Haskell binaries as above using ``rules_oci`` and Nixpkgs on macOS.

.. _current inability: https://github.com/NixOS/nixpkgs/issues/16696

Following these steps you should end up with a fairly lightweight Docker image, bringing the flexibility of Nix
as a Docker base image manager and the power of ``rules_haskell`` for your Haskell build together.

Cross-compilation
-----------------

Currently, ``rules_haskell`` only supports cross-compiling to ``arm`` on Linux.
Cross-compiling requires providing a cross-compiler, telling ``rules_haskell``
about it, and then requesting Bazel to build for the target platform.

Ideally, providing a cross-compiler would only require the advice in
`Picking a compiler`_. However, the case of ``arm`` requires to configure
a few aspects at this time. One has to make available the LLVM tools
to the compiler, emulation support needs to be set to enable
compilation of Template Haskell splices via an external interpreter,
and a compatible C cross-toolchain needs to be given as well for
linking. All of this is configured via Nix in the
`arm example`_, and the configuration can be copied as
is to other projects. Building the cross-compiler from this particular
configuration can be avoided by telling Nix to fetch it from the
`haskell.nix binary cache`_.

.. _arm example: https://github.com/tweag/rules_haskell/blob/master/examples/arm/arm-cross.nix
.. _haskell.nix binary cache: https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html#setting-up-the-binary-cache

To tell ``rules_haskell`` about the cross-compiler, we can register it
in the `WORKSPACE file <https://github.com/tweag/rules_haskell/blob/master/examples/arm/WORKSPACE>`_. ::

  load(
      "@rules_haskell//haskell:nixpkgs.bzl",
      "haskell_register_ghc_nixpkgs",
  )

  haskell_register_ghc_nixpkgs(
      name = "aarch64",
      version = "8.10.4",
      nix_file = "//:arm-cross.nix",
      attribute_path = "ghc-aarch64",
      static_runtime = True,
      exec_constraints = [
          "@platforms//cpu:x86_64",
          "@platforms//os:linux",
      ],
      target_constraints = [
          "@platforms//cpu:aarch64",
          "@platforms//os:linux",
      ],
      repository = "@nixpkgs",
  )

This rule indicates the Nix file and the Nix attribute path
to reach the cross-compiler. It says to link a static
runtime because the cross-compiler doesn't provide dynamic variants
of the core libraries. And finally, it specifies the execution and
target platform constraints. More information on platform constraints
and cross-compilation with Bazel can be found `here <https://docs.bazel.build/versions/master/platforms-intro.html>`_.

When using rules that depend on Cabal, ``rules_haskell`` also
needs a compiler targeting the execution platform, so the ``Setup.hs``
scripts can be executed. ::

  haskell_register_ghc_nixpkgs(
      name = "x86",
      version = "8.10.4",
      attribute_path = "haskell.compiler.ghc8102",
      exec_constraints = [
          "@platforms//cpu:x86_64",
          "@platforms//os:linux",
      ],
      target_constraints = [
          "@platforms//cpu:x86_64",
          "@platforms//os:linux",
      ],
      repository = "@nixpkgs",
  )

Similarly, we need to register the native and cross-toolchains for C. ::

  nixpkgs_cc_configure(
      name = "nixpkgs_config_cc_x86",
      exec_constraints = [
          "@platforms//cpu:x86_64",
          "@platforms//os:linux",
      ],
      repository = "@nixpkgs",
      target_constraints = [
          "@platforms//cpu:x86_64",
          "@platforms//os:linux",
      ],
  )

  nixpkgs_cc_configure(
      name = "nixpkgs_config_cc_arm",
      attribute_path = "cc-aarch64",
      exec_constraints = [
          "@platforms//cpu:x86_64",
          "@platforms//os:linux",
      ],
      nix_file = "//:arm-cross.nix",
      repository = "@nixpkgs",
      target_constraints = [
          "@platforms//cpu:aarch64",
          "@platforms//os:linux",
      ],
  )

Having the toolchains registered, the last remaining bit is telling
Bazel for which platform to build. Building for ``arm`` requires
declaring the platform in the `BUILD <https://github.com/tweag/rules_haskell/blob/master/examples/arm/BUILD.bazel>`_ file. ::

  platform(
      name = "linux_aarch64",
      constraint_values = [
          "@platforms//os:linux",
          "@platforms//cpu:aarch64",
      ],
  )

Then we can invoke ::

  bazel build --platforms=//:linux_aarch64 --incompatible_enable_cc_toolchain_resolution

to create the ``arm`` artifact. The flag ``--incompatible_enable_cc_toolchain_resolution``
is necessary to have Bazel use the platforms mechanism to select the C toolchains.
