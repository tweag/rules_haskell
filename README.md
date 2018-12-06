# Hazel: Automatic Bazel rules for third-party Haskell dependencies

[![CircleCI](https://circleci.com/gh/FormationAI/hazel/tree/master.svg?style=svg)](https://circleci.com/gh/FormationAI/hazel/tree/master)

Hazel is a Bazel framework that manages build rules for third-party Haskell
dependencies.

Rather than manually writing BUILD files (and checking them into your source
repository), Hazel only requires you to specify the set of third-party
dependencies and their versions.  Hazel autogenerates everything else at build
time, including automatically downloading Cabal packages from Hackage,
parsing their .cabal files and creating Haskell build rules.

Hazel uses the [`rules_haskell`](https://github.com/tweag/rules_haskell)
package.

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


Then, add some `rules_haskell`-related boilerplate to your `WORKSPACE` file,
as described in their
[`README`](https://github.com/tweag/rules_nixpkgs/blob/master/README.md):

```
http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-cd2ed701127ebf7f8f21d37feb1d678e4fdf85e5",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/cd2ed70.tar.gz"],
)
RULES_HASKELL_SHA = "f6f75f4b551202bff0a90e026cb572c181fac0d8"
http_archive(
    name = "io_tweag_rules_haskell",
    urls = ["https://github.com/tweag/rules_haskell/archive/"
            + RULES_HASKELL_SHA + ".tar.gz"],
    strip_prefix = "rules_haskell-" + RULES_HASKELL_SHA,
)
# Replace with a more recent commit, as appropriate
HAZEL_SHA=dca41ff6d9ce7a00e4af91ebe621f0c939e7e465
http_archive(
    name = "ai_formation_hazel",
    strip_prefix = "hazel-dca41ff6d9ce7a00e4af91ebe621f0c939e7e465",
    urls = ["https://github.com/FormationAI/hazel/archive/{}.tar.gz".format(
              HAZEL_COMMIT)],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_git_repository", "nixpkgs_package")

nixpkgs_git_repository(
    name = "nixpkgs",
    # A revision of 17.09 that contains ghc-8.2.2:
    revision = "c33c5239f62b4855b14dc5b01dfa3e2a885cf9ca",
)

nixpkgs_package(
    name = "ghc",
    repository = "@nixpkgs",
    attribute_path = "haskell.packages.ghc822.ghc",
    # NOTE: this uses the ghc bindist template provided by Hazel
    build_file = "@ai_formation_hazel//:BUILD.ghc",
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

register_toolchains("@ghc//:ghc")
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

For example:

```
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
