# Hazel: Automatic Bazel rules for third-party Haskell dependencies

[![CircleCI](https://circleci.com/gh/tweag/rules_haskell/tree/master.svg?style=svg)](https://circleci.com/gh/tweag/rules_haskell/tree/master)

Hazel is a Bazel framework that manages build rules for third-party Haskell
dependencies.

Rather than manually writing BUILD files (and checking them into your source
repository), Hazel only requires you to specify the set of third-party
dependencies and their versions.  Hazel autogenerates everything else at build
time, including automatically downloading Cabal packages from Hackage,
parsing their .cabal files and creating Haskell build rules.

Hazel uses the [`rules_haskell`](https://github.com/tweag/rules_haskell)
package for Haskell build rules. Hazel is now part of the `rules_haskell`
repository.

## Status
Hazel is still experimental, and its API is subject to change.  Most Hackage
packages will not yet build with it; however, a small number have been
verified so far as a proof of concept.

## Setting up a new project
First, run the `Stackage.hs` script to generate a list of all packages in a
particular LTS release or nightly snapshot:

```
./Stackage.hs lts-10.5 packages.bzl
```

```
./Stackage.hs nightly-2018-07-09 packages.bzl
```

That `packages.bzl` file should be checked into your repository.


Then, follow the instructions in the [`rules_haskell` `README`](../README.md)
to configure `rules_haskell`. Next, include Hazel as follows:

```
# Hazel is contained in the rules_haskell repository.
# Use the same git revision for rules_haskell and Hazel.
# Replace with a more recent commit, as appropriate
RULES_HASKELL_SHA = "2a4f902bd0911e479610abda895e6cdc9a72da41"
http_archive(
    name = "ai_formation_hazel",
    urls = ["https://github.com/tweag/rules_haskell/archive/"
            + RULES_HASKELL_SHA + ".tar.gz"],
    strip_prefix = "rules_haskell-" + RULES_HASKELL_SHA + "/hazel",
)
```

Finally, in `WORKSPACE`, load `packages.bzl` and feed its contents to `haskell_repositories` macro:

```
load("@ai_formation_hazel//:hazel.bzl", "hazel_repositories")
load("//:packages.bzl", "core_packages", "packages")

hazel_repositories(
    core_packages = core_packages,
    packages = packages)
```

## Using Hazel in build rules
The `hazel_repositories` macro creates a separate [external
dependency](https://docs.bazel.build/versions/master/external.html) for each
package.  It downloads the corresponding Cabal tarball from Hackage
and construct build rules for compiling the components of that package.

Note, that Haskell package names are case-sensitive while Bazel workspace names
are case-insensitive on case-insensitive file systems. For the generated
workspace names to be case-insensitive we include a hash of the original
package name. Query for targets `@all_hazel_packages//:haskell_{package_name}`
to determine the generated workspace name.

For example:

```
# Discover the generated external workspace for the vector package.
bazel query 'deps(@all_hazel_packages//:haskell_vector, 1)'
# --> @haskell_vector__820387517//:bzl

# Build all components of a single package, as well as all of its dependencies:
bazel build @haskell_vector__820387517//:all

# Build all components of all third-party packages:
bazel build @all_hazel_packages//:all
```

To depend on a third-party package in a `BUILD` file, use the macros provided by `hazel.bzl`:

```
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_test")
load("@ai_formation_hazel//:hazel.bzl", "hazel_library")

haskell_library(
    name = "Foo",
    srcs = ["Foo.hs"],
    deps = [
        hazel_library("base"),
    ],
)

haskell_test(
    name = "bar",
    srcs = ["Main.hs"],
    deps = [
        ":Foo",
        hazel_library("base"),
        hazel_library("vector"),
    ],
)
```
