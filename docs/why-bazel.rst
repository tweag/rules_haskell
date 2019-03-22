.. _why-bazel:

Is Bazel right for me?
======================

Nearly as many build tools exist as there are programming languages
out there. C++ has Autotools_/Make_, CMake_ and many others. Java has
Ant_, Maven_, Gradle_ and several more. Haskell has Cabal_, Stack_,
Shake_ and several more. Each of these originated in a given language
community but are in some cases generic enough to support building any
language. Are any of them the right choice for your use case? Should
you be combining several systems? That's what this document should
help you answer.

Rule of thumb
-------------

If a combination of the following apply, then you're better off using
Cabal_ or Stack_:

* your project is an independently publishable single library, or
  small set of libraries;
* your project is open source code and has at most small static
  assets (hence publishable on Hackage);
* your project is nearly entirely Haskell code with perhaps a little
  bit of C;
* your project has many dependencies on other packages found on
  Hackage but few if any system dependencies (like zlib, libpng etc);

Bazel works well for the following use cases:

* projects that cannot be hosted on Hackage (games with large static
  assets, proprietary code etc);
* projects with a very large amount of code hosted in a single
  repository;
* projects in which you or your team are writing code in two or more
  languages (e.g. Haskell/PureScript, or Haskell/Java, or
  Haskell/C++/FORTRAN);

Rationale
---------

For all the benefits it can bring, Bazel also has an upfront cost.
Don't pay that cost if the benefits don't justify it.

If you don't have much code to build, any build tool will do. Build
issues like lack of complete reproducibility are comparatively easier
to debug, and working around build system bugs by wiping the entire
build cache first is entirely viable in this particular case. So might
as well use low-powered Haskell-native build tools that ship with GHC.
You won't *need* sandboxed build actions to guarantee build system
correctness, completely hermetic builds for good reproducibility,
build caching, test result caching or distributed builds for faster
build and test times. Those features start to matter for larger
projects, and become essential for very large monorepos_.

Why exactly do these features matter?

* **Hermetic builds** are builds that do not take any part of the
  host's system configuration (set of installed system libraries and
  their versions, content of ``/etc``, OS version, etc) as an input.
  If all build actions are deterministic, hermeticity guarantees that
  builds are reproducible anywhere, anytime. More developers on
  a project means more subtly different system configurations to cope
  with. The more system configurations, the more likely that the build
  will fail in one of these configurations but not in others... Unless
  the build is completely hermetic.
* **Sandboxing build actions** guarantees that all inputs to all build
  actions are properly declared. This helps prevent build system
  correctness bugs, which are surprisingly and exceedingly common in
  most non-sandboxing build systems, especially as the build system
  becomes more complex. When a build system *might* be incorrect,
  users regularly have to wipe the entire build cache to work around
  issues. As the codebase becomes very large, rebuilding from scratch
  can cost a lot of CPU time.
* **Distributed build caches** make building the code from a fresh
  checkout trivially fast. Continuous integration populates the build
  cache at every branch push, so that building all artifacts from
  fresh checkouts seldom needs to actually build anything at all
  locally. In the common case, builds become network-bound instead of
  CPU-bound.
* **Distributed build action execution** mean that average build times
  can stay constant even as the codebase grows, because you can
  seamlessly distribute the build on more machines.
* **Test result caching** is the key to keeping continuous
  integration times very low. Only those tests that depend on code
  that was modified need be rerun.

On their own hermetic and sandboxed builds can already save quite
a few headaches. But crucially, without them one can't even hope to
have any of the other features that follow them above.

.. _Autotools: https://en.wikipedia.org/wiki/GNU_Build_System
.. _Make: https://en.wikipedia.org/wiki/Make_(software)
.. _CMake: https://cmake.org/
.. _Ant: https://ant.apache.org/
.. _Maven: https://maven.apache.org/index.html
.. _Gradle: https://gradle.org/
.. _Cabal: https://www.haskell.org/cabal/
.. _Stack: http://haskellstack.org/
.. _Shake: https://shakebuild.com/
.. _monorepos: https://en.wikipedia.org/wiki/Monorepo
