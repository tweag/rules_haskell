# Haskell rules for [Bazel][bazel]

[![CircleCI](https://circleci.com/gh/tweag/rules_haskell.svg?style=svg)](https://circleci.com/gh/tweag/rules_haskell)

**NOTE: this is alpha quality software. Please don't publicize widely.**

Bazel automates building and testing software. It scales to very large
multi-language projects. This project extends Bazel with build rules
for Haskell. Get started building your own project using these rules
wih the [setup script below](#setup).

**WORKSPACE rules:**

| Rule | Description |
| ---: | :--- |
| [`haskell_repositories`](#haskell_repositories) | Declare [external repositories][external-repositories] needed for rules_haskell |

**BUILD rules:**

| Rule | Description |
| ---: | :--- |
| [`haskell_library`](#haskell_library) | Build a library from Haskell source. |
| [`haskell_binary`](#haskell_binary) | Build an executable from Haskell source. |
| [`haskell_test`](#haskell_test) | Run a test suite. |
| [`haskell_doc`](#haskell_doc) | Create API documentation. |
| [`haskell_toolchain`](#haskell_toolchain) | Declare a compiler toolchain. |
| [`haskell_cc_import`](#haskell_cc_import) | Import a prebuilt shared library. |
| [`cc_haskell_import`](#cc_haskell_import) | Expose all transitive shared object libraries for haskell dependency. |

[bazel]: https://bazel.build/
[bazel-getting-started]: https://docs.bazel.build/versions/master/getting-started.html
[bazel-cli]: https://docs.bazel.build/versions/master/command-line-reference.html
[external-repositories]: https://docs.bazel.build/versions/master/external.html
[nix]: https://nixos.org/nix

## Setup

You'll need [Bazel >= 0.8.1][bazel-getting-started] installed.

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
new_local_package(
  name = "my_ghc",
  path = "/usr/local", # Change path accordingly.
  build_file_content = """
package(default_visibility = ["//visibility:public"])
filegroup (name = "bin", srcs = glob(["bin/ghc*"]))
  """
)
```

## For `rules_haskell` developers

To run the test suite for these rules, you'll need [Nix][nix]
installed. To build and run tests locally, execute:

```
$ bazel test //...
```

## Rules

See https://haskell.build for the reference documentation on provided
rules. Using [./serve-docs.sh](./serve-docs.sh), you can also view
this documentation locally.

## Language interop

We may be supporting interop with other languages in one way or
another. Please see languages listed below about how.

### C/C++

C/C++ libraries can be specified as depnedencies. Importing prebuilt
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
