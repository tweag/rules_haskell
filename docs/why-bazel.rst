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
