.. _guide:

Introduction to Bazel: Building a Haskell project
=================================================

In this tutorial, you'll learn the basics of building Haskell
applications with Bazel. You will set up your workspace and build
a simple Haskell project that illustrates key Bazel concepts, such as
targets and ``BUILD.bazel`` files. After completing this tutorial, take
a look at :ref:`Common Haskell build use cases <use-cases>` for
information on more advanced concepts such as writing and running
Haskell tests.

What you'll learn
-----------------

In this tutorial you'll learn how to:

* build a target,
* visualize the project's dependencies,
* split the project into multiple targets and packages,
* control target visibility across packages,
* reference targets through labels.

Before you begin
----------------

On a Unix system you will need the following tools installed.

* ``gcc``
* ``libffi``
* ``libgmp``
* ``libtinfo5``
* ``make``
* ``python3`` (``python`` also needs to be available in ``$PATH``. Depending on your distribution, this might require installing the ``python`` meta-package, which might use Python 2 or 3, ``rules_haskell`` works with both.)

On Ubuntu you can obtain them by installing the following packages. ::

  build-essential libffi-dev libgmp-dev libtinfo5 libtinfo-dev python python3

On Windows you will need.

- ``msys2``
- ``python3``

Next, `install Bazel`_ if you don't have it installed already. Then, retrieve
the ``rules_haskell`` GitHub repository::

  git clone https://github.com/tweag/rules_haskell/

The sample project for this tutorial is in the ``tutorial``
directory and is structured as follows::

  rules_haskell
  └── tutorial
     ├── WORKSPACE
     ├── main
     │  ├── BUILD.bazel
     │  └── Main.hs
     └── lib
        ├── BUILD.bazel
        └── Bool.hs

The first thing to do is to::

  $ cd tutorial

If you use the ``NixOS`` distribution, also run the following command::

  $ echo 'test --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host' >> .bazelrc.local

Build with Bazel
----------------

Set up the workspace
^^^^^^^^^^^^^^^^^^^^

Before you can build a project, you need to set up its workspace.
A workspace is a directory that holds your project's source files and
Bazel's build outputs. It also contains files that Bazel recognizes as
special:

* the ``WORKSPACE`` file, which identifies the directory and its
  contents as a Bazel workspace and lives at the root of the project's
  directory structure,

* one or more ``BUILD.bazel`` files, which tell Bazel how to build different
  parts of the project. (A directory within the workspace that
  contains a ``BUILD.bazel`` file is a *package*. You will learn about
  packages later in this tutorial.)

To designate a directory as a Bazel workspace, create a file
named ``WORKSPACE`` in that directory.
This file defines `external dependencies`_.

When Bazel builds the project, all inputs and dependencies must be in
the same workspace. Files residing in different workspaces are
independent of one another unless linked, which is beyond the scope of
this tutorial.

Understand the WORKSPACE file
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The file ``tutorial/WORKSPACE`` defines how to obtain ``rules_haskell``.
This file only works within the ``rules_haskell``
repository: for your own project, run the start_ script
to create a ``WORKSPACE`` file that makes ``rules_haskell`` available
by `downloading it`_.

The ``WORKSPACE`` file also defines how to obtain the compiler toolchain,
as detailed in section `picking a compiler`_.

Understand the BUILD file
^^^^^^^^^^^^^^^^^^^^^^^^^

It is recommended to use a ``.bazel`` extension for each ``BUILD`` file to
avoid clashing with files or folders already using that name.

A ``BUILD.bazel`` file contains several different types of instructions for
Bazel. The most important type is the *build rule*, which tells Bazel
how to build the desired outputs, such as executable binaries or
libraries. Each instance of a build rule in the ``BUILD.bazel`` file is
called a *target* and points to a specific set of source files and
dependencies. A target can also point to other targets.

Take a look at the ``BUILD.bazel`` file in the ``tutorial/lib`` directory::

  haskell_library(
      name = "booleans",
      srcs = ["Bool.hs"],
  )

In our example, the ``booleans`` target instantiates the
`haskell_library`_ rule. The rule tells Bazel to build a reusable
(statically or dynamically linked) library from the ``Bool.hs`` source
file with no dependencies.

The attributes in the target explicitly state its dependencies and
options. While the ``name`` attribute is mandatory, many are optional.
For example, in the ``booleans`` target, ``name`` is self-explanatory,
and ``srcs`` specifies the source file(s) from which Bazel builds the
target.

Build the project
^^^^^^^^^^^^^^^^^

Let's build your sample project. Run the following command::

  $ bazel build //lib:booleans

Notice the target label - the ``//lib:`` part is the location of our
``BUILD.bazel`` file relative to the root of the workspace, and ``booleans``
is what we named that target in the ``BUILD.bazel`` file. (You will learn
about target labels in more detail at the end of this tutorial.)

Bazel produces output similar to the following::

  INFO: Found 1 target...
  Target //lib:booleans up-to-date:
    bazel-bin/lib/libZSbooleans/libZSbooleans.conf
    bazel-bin/lib/libZSbooleans/package.cache
  INFO: Elapsed time: 2.288s, Critical Path: 0.68s

Congratulations, you just built your first Bazel target! Bazel places
build outputs in the ``bazel-bin`` directory at the root of the
workspace. Browse through its contents to get an idea for Bazel's
output structure.

Review the dependency graph
^^^^^^^^^^^^^^^^^^^^^^^^^^^

A successful build has all of its dependencies explicitly stated in
the ``BUILD.bazel`` file. Bazel uses those statements to create the
project's dependency graph, which enables accurate incremental builds.

Let's visualize our sample project's dependencies. First, generate
a text representation of the dependency graph (run the command at the
workspace root)::

  bazel query --nohost_deps --noimplicit_deps \
    'deps(//lib:booleans)' --output graph

The above command tells Bazel to look for all dependencies for the
target ``//lib:booleans`` (excluding host and implicit dependencies)
and format the output as a graph.

Then, paste the text into GraphViz_.

On Ubuntu, you can view the graph locally by installing GraphViz and the xdot
Dot Viewer::

  sudo apt update && sudo apt install graphviz xdot

Then you can generate and view the graph by piping the text output above
straight to xdot::

  xdot <(bazel query --nohost_deps --noimplicit_deps \
           'deps(//lib:booleans)' --output graph)

As you can see, the first stage of the sample project has a single
target that builds a single source file with no additional
dependencies:

.. digraph:: booleans

   node [shape=box];
   "//lib:booleans"
   "//lib:booleans" -> "//lib:Bool.hs"
   "//lib:Bool.hs"

Now that you have set up your workspace, built your project, and
examined its dependencies, let's add some complexity.

Refine your Bazel build
-----------------------

While a single target is sufficient for small projects, you may want
to split larger projects into multiple targets and packages to allow
for fast incremental builds (that is, only rebuild what's changed) and
to speed up your builds by building multiple parts of a project at
once.

Specify multiple build targets
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's split our sample project build into two targets. Take a look at
the ``BUILD.bazel`` files in the ``tutorial/lib`` and ``tutorial/main``
directories. The contents of both files could have been kept in
a single ``BUILD.bazel`` as follows::

  haskell_library(
      name = "booleans",
      srcs = ["Bool.hs"],
  )

  haskell_toolchain_library(name = "base")

  haskell_binary(
      name = "demorgan",
      srcs = ["Main.hs"],
      compiler_flags = ["-threaded"],
      deps = [":base", ":booleans"],
  )

With this single ``BUILD.bazel`` file, Bazel first builds the ``booleans``
library (using the `haskell_library`_ rule), then the ``demorgan``
binary (which as an example uses the ``booleans`` library to check one
of the De Morgan laws). The ``deps`` attribute in the ``demorgan``
target tells Bazel that the ``:booleans`` library is required to build
the ``demorgan`` binary. The binary also requires the ``base``
built-in library that ships with GHC, to perform I/O among other
things. Libraries like ``base``, ``bytestring`` and others that ship
with GHC are special in that they are prebuilt outside of Bazel. To
import them as regular targets, we use the `haskell_toolchain_library`_ rule.

Let's build this new version of our project::

  $ bazel build //main:demorgan

Bazel produces output similar to the following::

  INFO: Found 1 target...
  Target //main:demorgan up-to-date:
    bazel-bin/main/demorgan
  INFO: Elapsed time: 2.728s, Critical Path: 1.23s

Now test your freshly built binary::

  $ bazel-bin/main/demorgan

Or alternatively::

  $ bazel run //main:demorgan

If you now modify ``Bool.hs`` and rebuild the project, Bazel will
usually only recompile that file.

Looking at the dependency graph:

.. digraph:: demorgan

  node [shape=box];
  "//main:demorgan"
  "//main:demorgan" -> "//main:base\n//main:Main.hs"
  "//main:demorgan" -> "//lib:booleans"
  "//lib:booleans"
  "//lib:booleans" -> "//lib:Bool.hs"
  "//lib:Bool.hs"
  "//main:base\n//main:Main.hs"

You have now built the project with two targets. The ``demorgan``
target builds one source file and depends on one other target
(``//lib:booleans``), which builds one additional source file.

Use multiple packages
^^^^^^^^^^^^^^^^^^^^^

Let’s now split the project into multiple packages.

Notice that we actually have two sub-directories, and each contains
a ``BUILD.bazel`` file. Therefore, to Bazel, the workspace contains two
packages, ``lib`` and ``main``.

Take a look at the ``lib/BUILD.bazel`` file::

  haskell_library(
      name = "booleans",
      srcs = ["Bool.hs"],
      visibility = ["//main:__pkg__"],
  )

And at the ``main/BUILD.bazel`` file::

  haskell_toolchain_library(name = "base")

  haskell_binary(
      name = "demorgan",
      srcs = ["Main.hs"],
      compiler_flags = ["-threaded"],
      deps = [":base", "//lib:booleans"],
  )

As you can see, the ``demorgan`` target in the ``main`` package
depends on the ``booleans`` target in the ``lib`` package (hence the
target label ``//lib:booleans``) - Bazel knows this through the
``deps`` attribute.

Notice that for the build to succeed, we make the ``//lib:booleans``
target in ``lib/BUILD.bazel`` explicitly visible to targets in
``main/BUILD.bazel`` using the ``visibility`` attribute. This is because by
default targets are only visible to other targets in the same
``BUILD.bazel`` file. (Bazel uses target visibility to prevent issues such
as libraries containing implementation details leaking into public
APIs.)

You have built the project as two packages with three targets and
understand the dependencies between them.

Use labels to reference targets
-------------------------------

In ``BUILD.bazel`` files and at the command line, Bazel uses *labels* to
reference targets - for example, ``//main:demorgan`` or
``//lib:booleans``. Their syntax is::

  //path/to/package:target-name

If the target is a rule target, then ``path/to/package`` is the path
to the directory containing the ``BUILD.bazel`` file, and ``target-name`` is
what you named the target in the ``BUILD.bazel`` file (the ``name``
attribute). If the target is a file target, then ``path/to/package``
is the path to the root of the package, and ``target-name`` is the
name of the target file, including its full path.

When referencing targets within the same package, you can skip the
package path and just use ``//:target-name``. When referencing targets
within the same ``BUILD.bazel`` file, you can even skip the ``//`` workspace
root identifier and just use ``:target-name``.

Further reading
---------------

Congratulations! You now know the basics of building a Haskell project
with Bazel. Next, read up on :ref:`Common Haskell build use cases
<use-cases>` and have a look `cat_hs`_ for an example build
description of a full application. Then, check out the following:

* `External Dependencies`_ to learn more about working with local and
   remote repositories.

* The `Build Encyclopedia`_ to learn more about Bazel.

* The `C++ build tutorial`_ to get started with building C++
  applications with Bazel.

* The `Java build tutorial`_ to get started with building Java
  applications with Bazel.

* The `Android application tutorial`_ to get started with building
  mobile applications for Android with Bazel.

* The `iOS application tutorial`_ to get started with building mobile
  applications for iOS with Bazel.

Happy building!

.. note:: This tutorial is adapted from the Bazel `C++ build tutorial`_.

.. _cat_hs: https://github.com/tweag/rules_haskell/tree/master/examples/cat_hs
.. _install Bazel: https://docs.bazel.build/versions/master/install.html
.. _haskell_binary: http://api.haskell.build/haskell/haskell.html#haskell_binary
.. _haskell_toolchain_library: http://api.haskell.build/haskell/haskell.html#haskell_toolchain_library
.. _haskell_library: http://api.haskell.build/haskell/haskell.html#haskell_library
.. _graphviz: https://www.graphviz.org/
.. _start: https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#starting-a-new-project
.. _downloading it: https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#making-rules-haskell-available
.. _picking a compiler: https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#picking-a-compiler
.. _external dependencies: https://docs.bazel.build/versions/master/external.html
.. _build encyclopedia: https://docs.bazel.build/versions/master/be/overview.html
.. _C++ build tutorial: https://docs.bazel.build/versions/master/tutorial/cpp.html
.. _Java build tutorial: https://docs.bazel.build/versions/master/tutorial/java.html
.. _Android application tutorial: https://docs.bazel.build/versions/master/tutorial/android-app.html
.. _iOS application tutorial: https://docs.bazel.build/versions/master/tutorial/ios-app.html
