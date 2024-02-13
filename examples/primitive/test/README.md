Test Suite
=======================

The test suite for `primitive` cannot be included in the same package
as `primitive` itself. The test suite depends on `QuickCheck`, which
transitively depends on `primitive`. To break up this dependency cycle,
the test suite lives here in its own unpublished package.

To accelerates builds of the test suite, it is recommended to use
`cabal new-build`, which will use the pass the flags specified in
the `cabal.project` file to build `quickcheck-classes`. From the
root directory of `primitive`, run the following command to build
the test suite:

    cabal new-build test --enable-tests

