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

.. todo::

   write up the reasoning behind rules of thumb above.

.. _Autotools: https://en.wikipedia.org/wiki/GNU_Build_System
.. _Make: https://en.wikipedia.org/wiki/Make_(software)
.. _CMake: https://cmake.org/
.. _Ant: https://ant.apache.org/
.. _Maven: https://maven.apache.org/index.html
.. _Gradle: https://gradle.org/
.. _Cabal: https://www.haskell.org/cabal/
.. _Stack: http://haskellstack.org/
.. _Shake: https://shakebuild.com/
