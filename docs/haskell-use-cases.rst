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
----------------------------------

First of all, the ``WORKSPACE`` file must specify how to obtain
rules_haskell. To use a released version, do the following::

  load(
      "@bazel_tools//tools/build_defs/repo:http.bzl",
      "http_archive"
  )

  http_archive(
      name = "rules_haskell",
      strip_prefix = "rules_haskell-0.12",
      urls = ["https://github.com/tweag/rules_haskell/archive/v0.12.tar.gz"],
      sha256 = "56a8e6337df8802f1e0e7d2b3d12d12d5d96c929c8daecccc5738a0f41d9c1e4",
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
      "@rules_haskell//haskell:defs.bzl",
      "haskell_register_ghc_bindists",
  )

  haskell_register_ghc_bindists(
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
.. _ghc_bindist: http://api.haskell.build/haskell/ghc_bindist.html#ghc_bindist
.. _haskell.org: https://haskell.org
.. _haskell_binary: http://api.haskell.build/haskell/haskell.html#haskell_binary
.. _haskell_library: http://api.haskell.build/haskell/haskell.html#haskell_library
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

Building Cabal packages
-----------------------

If you depend on third-party code hosted on Hackage_, these will have
a build script that uses the Cabal_ framework. Bazel can build these
with the `haskell_cabal_library`_ and `haskell_cabal_binary`_ rules.
However, you seldom want to use them directly. Cabal packages
typically have many dependencies, which themselves have dependencies
and so on. It is tedious to describe all of these dependencies to
Bazel by hand. You can use the `stack_snapshot`_ workspace rule
to download the source of all necessary dependencies from Hackage,
and extract a dependency graph from a Stackage_ snapshot.

These rules are meant only to interoperate with third-party code. For
code under your direct control, prefer using one of the core Haskell
rules, which have more features, are more efficient and more
customizable.

.. _Cabal: https://haskell.org/cabal
.. _Hackage: https://hackage.haskell.org
.. _Stackage: https://stackage.org
.. _haskell_cabal_library: https://api.haskell.build/haskell/cabal.html#haskell_cabal_library
.. _haskell_cabal_binary: https://api.haskell.build/haskell/cabal.html#haskell_cabal_binary
.. _stack_snapshot: https://api.haskell.build/haskell/cabal.html#stack_snapshot

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

.. _haskell_toolchain_library: http://api.haskell.build/haskell/haskell.html#haskell_toolchain_library

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

.. _haskell_doc: http://api.haskell.build/haskell/haddock.html#haskell_doc

Linting your code
-----------------

The `haskell_lint`_ rule does not build code but runs the GHC
typechecker on all listed dependencies. Warnings are treated as
errors.

Alternatively, you can directly check a target using

.. code-block:: console

  $ bazel build //my/haskell:target \
      --aspects @rules_haskell//haskell:defs.bzl%haskell_lint_aspect

.. _haskell_lint: http://api.haskell.build/haskell/lint.html#haskell_lint

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

Persistent Worker Mode (experimental)
-------------------------------------

Bazel supports the special `persistent worker mode`_ when instead of calling the compiler
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

Containerization with rules_docker
-------------------------------------

Making use of both ``rules_docker`` and ``rules_nixpkgs``, it's possible to containerize
``rules_haskell`` ``haskell_binary`` build targets for deployment. In a nutshell, first we must use
``rules_nixpkgs`` to build a ``dockerTools.buildLayeredImage`` target with the basic library dependencies
required to run a typical haskell binary. Thereafter, we can use ``rules_docker`` to use this as
a base image upon which we can layer a bazel built haskell binary.

Step one is to ensure you have all the necessary ``rules_docker`` paraphernalia loaded in your ``WORKSPACE``
file: ::

  http_archive(
      name = "io_bazel_rules_docker",
      sha256 = "df13123c44b4a4ff2c2f337b906763879d94871d16411bf82dcfeba892b58607",
      strip_prefix = "rules_docker-0.13.0",
      urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.13.0/rules_docker-v0.13.0.tar.gz"],
  )

  load("@io_bazel_rules_docker//toolchains/docker:toolchain.bzl", docker_toolchain_configure="toolchain_configure")

To make full use of post-build ``rules_docker`` functionality, we'll want to make sure this is set
to the docker binary's location ::

  docker_toolchain_configure(
      name = "docker_config",
      docker_path = "/usr/bin/docker"
  )

  load("@io_bazel_rules_docker//container:container.bzl", "container_load")

  load("@io_bazel_rules_docker//repositories:repositories.bzl", container_repositories = "repositories")
  container_repositories()

  load("@io_bazel_rules_docker//repositories:deps.bzl", container_deps = "deps")
  container_deps()

Then we're ready to specify a base image built using the ``rules_nixpkgs`` ``nixpkgs_package`` rule for ``rules_docker`` to layer its products on top of ::

  nixpkgs_package(
      name = "raw-haskell-base-image",
      repository = "//nixpkgs:default.nix",
      nix_file = "//nixpkgs:haskellBaseImageDocker.nix", # See below for how to define this
      build_file_content = """
  package(default_visibility = [ "//visibility:public" ]),
  exports_file(["image"])
      """,
  )

And finally use the ``rules_docker`` ``container_load`` functionality to grab the docker image built by the previous ``raw-haskell-base-image`` target ::

  container_load(
      name = "haskell-base-image",
      file = "@raw-haskell-base-image//:image",
  )

Step two requires that we specify our nixpkgs/haskellBaseImageDocker.nix file as follows ::

  # nixpkgs is provisioned by rules_nixpkgs for us which we set to be ./default.nix
  with import <nixpkgs> { system = "x86_64-linux"; };

  # Build the base image.
  # The output of this derivation will be a docker archive in the same format as
  # the output of `docker save` that we can feed to
  # [container_load](https://github.com/bazelbuild/rules_docker#container_load)
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

Step three pulls all this together in a build file to actually assemble our final docker image. In a BUILD.bazel file, we'll need the following ::

  load("@io_bazel_rules_docker//cc:image.bzl", "cc_image")
  load("@io_bazel_rules_docker//container:container.bzl", "container_push")

  haskell_binary(
      name = "my_binary,
      srcs = ["Main.hs"],
      compiler_flags = [
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

  cc_image(
      name = "my_binary_image",
      base = "@haskell-base-image//image",
      binary = ":my_binary",
      ports = [ "8000/tcp" ],
      creation_time = "{BUILD_TIMESTAMP}",
      stamp = True,
  )

And you may want to use ``rules_docker`` to push your docker image as follows ::

  container_push(
      name = "my_binary_push",
      image = ":my_binary_image",
      format = "Docker",
      registry = "gcr.io", # For example using a GCP GCR repository
      repository = "$project-name-here/$my_binary_image_label",
      tag = "{BUILD_USER}",
 )

*n.b* Due to the `current inability`_ of nix to be used on macOS (darwin) for building docker images, it's currently
not possible to build docker images for haskell binaries as above using rules_docker and nixpkgs on macOS.

.. _current inability: https://github.com/NixOS/nixpkgs/issues/16696

Following these steps you should end up with a fairly lightweight docker image, bringing the flexibility of nix
as a docker base image manager and the power of ``rules_haskell`` for your haskell build together.
