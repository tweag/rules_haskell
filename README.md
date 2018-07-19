# Haskell rules for [Bazel][bazel]

[![CircleCI](https://circleci.com/gh/tweag/rules_haskell.svg?style=svg)](https://circleci.com/gh/tweag/rules_haskell)

Bazel automates building and testing software. It scales to very large
multi-language projects. This project extends Bazel with build rules
for Haskell. Get started building your own project using these rules
wih the [setup script below](#setup).

## Rule summary

The full reference documentation for rules is at https://haskell.build.

**WORKSPACE rules:**

| Rule | Description |
| ---: | :--- |
| [`haskell_repositories`](https://haskell.build/haskell/repositories.html#haskell_repositories) | Declare [external repositories][external-repositories] needed for rules_haskell |
| [`ghc_bindist`](https://haskell.build/haskell/ghc_bindist.html#import_ghc_bindist) | Setup a binary distribution of GHC |

**BUILD rules:**

| Rule | Description |
| ---: | :--- |
| [`haskell_library`](https://haskell.build/haskell/haskell.html#haskell_library) | Build a library from Haskell source. |
| [`haskell_binary`](https://haskell.build/haskell/haskell.html#haskell_binary) | Build an executable from Haskell source. |
| [`haskell_test`](https://haskell.build/haskell/haskell.html#haskell_test) | Run a test suite. |
| [`haskell_doc`](https://haskell.build/haskell/haddock.html#haskell_doc) | Create API documentation. |
| [`haskell_toolchain`](https://haskell.build/haskell/toolchain.html#haskell_toolchain) | Declare a compiler toolchain. |
| [`haskell_cc_import`](https://haskell.build/haskell/cc.html#haskell_cc_import) | Import a prebuilt shared library. |
| [`cc_haskell_import`](https://haskell.build/haskell/cc.html#cc_haskell_import) | Expose all transitive shared object libraries for haskell dependency. |

[bazel]: https://bazel.build/
[bazel-getting-started]: https://docs.bazel.build/versions/master/getting-started.html
[bazel-cli]: https://docs.bazel.build/versions/master/command-line-reference.html
[external-repositories]: https://docs.bazel.build/versions/master/external.html
[nix]: https://nixos.org/nix

## Setup

You'll need [Bazel >= 0.14.0][bazel-getting-started] installed.

### The easy way

In a fresh directory, run:

```console
$ curl https://haskell.build/start | sh
```

This will generate initial `WORKSPACE` and `BUILD` files for you. See the
[examples](./tests) and the [API reference](#Rules) below to adapt these for
you project. Then,

```console
$ bazel build //...    # Build all targets
$ bazel test //...     # Run all tests
```

You can learn more about Bazel's command line
syntax [here][bazel-cli]. Common [commands][bazel-cli-commands] are
`build`, `test`, `run` and `coverage`.

[bazel-cli-commands]: https://docs.bazel.build/versions/master/command-line-reference.html#commands

### Doing it manually

Add the following to your `WORKSPACE` file, and select a `$VERSION`
(or even an arbitrary commit hash) accordingly.

```bzl
http_archive(
  name = "io_tweag_rules_haskell",
  strip_prefix = "rules_haskell-$VERSION",
  urls = ["https://github.com/tweag/rules_haskell/archive/v$VERSION.tar.gz"],
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

register_toolchains("//:ghc")
```

Then, add this to your root `BUILD` file:

```bzl
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_toolchain",
)

haskell_toolchain(
  name = "ghc",
  version = "8.2.2",
  tools = "@my_ghc//:bin",
)
```

The `haskell_toolchain` rule instantiation brings a GHC compiler in
scope. It assumes that an [external repository][external-repositories]
called `@my_ghc` was defined, pointing to an installation of GHC. The
recommended option is to provision GHC using Nix, but you can also
point to an existing local installation somewhere in your filesystem.
Using Nix, this is done by adding the following to your `WORKSPACE`
file:

```bzl
nixpkgs_package(
  name = "my_ghc",
  attribute_path = "haskell.compiler.ghc822"
)
```

Alternatively, you can point to an existing global installation:

```bzl
new_local_repository(
  name = "my_ghc",
  path = "/usr/local", # Change path accordingly.
  build_file_content = """
package(default_visibility = ["//visibility:public"])
filegroup (name = "bin", srcs = glob(["bin/ghc*"]))
  """
)
```

## Examples

See [rules_haskell_examples][] for examples of using these rules.

[rules_haskell_examples]: https://github.com/tweag/rules_haskell_examples

## For `rules_haskell` developers

To run the test suite for these rules, you'll need [Nix][nix]
installed. First, from the project’s folder start a pure nix shell:

```
$ nix-shell --pure shell.nix
```

This will make sure that bazel has the exact same environment
on every development system (`python`, `ghc`, `go`, …).

To build and run tests locally, execute:

```
$ bazel test //...
```

Skylark code in this project is formatted according to the output of
[buildifier]. You can check that the formatting is correct using:

```
$ bazel test --config lint //...
```

If tests fail then run the following to fix the formatting:

```
$ bazel run --direct_run //skylark:buildifier **/*.bzl **/BUILD
```

[buildifier]: https://github.com/bazelbuild/buildtools/tree/master/buildifier

## Rules

See https://haskell.build for the reference documentation on provided
rules. Using [./serve-docs.sh](./serve-docs.sh), you can also view
this documentation locally.

## Language interop

We may be supporting interop with other languages in one way or
another. Please see languages listed below about how.

### C/C++

C/C++ libraries can be specified as dependencies. Importing prebuilt
libraries and exporting Haskell libraries as C/C++ dependencies
currently requires the `haskell_cc_import` and `cc_haskell_import`
rules. These are temporary workarounds to Bazel limitations.

### Java

You can supply `java_*` rule targets in `deps` of
[haskell_binary](#haskell_binary) and
[haskell_library](#haskell_library). This will make jars produced by
those dependencies available during Haskell source compilation phase
(i.e. not during linking &c. but it's subject to change) and set the
CLASSPATH for that phase as well.

## Building Cabal packages published on Hackage

This repository contains no special support for building Cabal
packages. This is provided by downstream rule sets. We
recommend [Hazel][hazel] for generating rules to build packages
published on Hackage, or part of Stackage snapshots, using Bazel.

[hazel]: https://github.com/formationai/hazel

## Troubleshooting `rules_haskell` development

### `bazel` fails because some executable cannot be found

Make sure you run your build in a pure nix shell
(`nix-shell --pure shell.nix`). If it still doesn’t build,
it is likely a bug.

### A Haskell dependency fails with strange error messages

If you get cabal error messages the likes of:

```
CallStack (from HasCallStack):
  dieNoWrap, called at libraries/Cabal/Cabal/Distribution/Utils/LogProgress.hs:61:9 in Cabal-2.0.1.0:Distribution.Utils.LogProgress
Error:
    The following packages are broken because other packages they depend on are missing. These broken packages must be rebuilt before they can be used.
installed package lens-labels-0.2.0.1 is broken due to missing package profunctors-5.2.2-HzcVdviprlKb7Ap1woZu4, tagged-0.8.5-HviTdonkllN1ZD6he1Zn8I
```

you’ve most likely hit GHC’s
[infamous non-deterministic library ID bug](https://nixos.org/nixpkgs/manual/#how-to-recover-from-ghcs-infamous-non-deterministic-library-id-bug).

