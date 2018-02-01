# Haskell rules for [Bazel][bazel]

[![CircleCI](https://circleci.com/gh/tweag/rules_haskell.svg?style=svg)](https://circleci.com/gh/tweag/rules_haskell)

**NOTE: this is alpha quality software. Please don't publicize widely.**

To use these rules, you'll need [Bazel >= 0.8.1][bazel-install]. To run
tests, you'll furthermore need [Nix][nix] installed. Once Nix is installed,
this command will bring Bazel into scope:

```
$ nix-env -iA nixpkgs.bazel
```

To build all targets, execute this from root of the repo:

```
$ bazel build //...
```

To run tests locally, execute:

```
$ bazel test //...
```

You can learn more about Bazel's command line syntax [here][bazel-cli].

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
[bazel-install]: https://docs.bazel.build/versions/master/install.html
[bazel-cli]: https://docs.bazel.build/versions/master/command-line-reference.html
[external-repositories]: https://docs.bazel.build/versions/master/external.html
[nix]: https://nixos.org/nix

## Setup

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

## Rules

### haskell_binary

Build an executable from Haskell source.

```bzl
haskell_binary(name, srcs, deps)
```

#### Example

```bzl
haskell_binary(
    name = "main",
    srcs = ["Main.hs", "Other.hs"],
    deps = ["//lib:some_lib"]
)
```

#### Attributes

| Attribute | Type | Description |
| --------: | :--- | :---------- |
| `name` | `Name, required` | A unique name for this target |
| `srcs` | `Label list, required` | Haskell source files (`.hs` or `.hsc`) |
| `deps` | `Label list, required` | List of other Haskell libraries to be linked to this target |
| `src_strip_prefix` | `String, optional` | Directory in which module hierarchy starts |
| `compiler_flags` | `String list, optional` | Flags to pass to Haskell compiler |
| `prebuilt_dependencies` | `String list, optional` | Non-Bazel supplied Cabal dependencies |
| `main` | `String, optional` | Location of `main` function. Default: `"Main.main"` |

### haskell_library

Build a library from Haskell source.

```bzl
haskell_library(name, srcs, deps)
```

#### Example

```bzl
haskell_library(
    name = 'hello_lib',
    srcs = glob(['hello_lib/**/*.hs']),
    deps = ["//hello_sublib:lib"],
	prebuilt_dependencies = ["base", "bytestring"],
)
```

#### Attributes

| Attribute | Type | Description |
| --------: | :--- | :---------- |
| `name` | `Name, required` | A unique name for this target |
| `srcs` | `Label list, required` | Haskell source files (`.hs` or `.hsc`) |
| `deps` | `Label list, required` | List of other Haskell libraries to be linked to this target |
| `src_strip_prefix` | `String, optional` | Directory in which module hierarchy starts |
| `compiler_flags` | `String list, optional` | Flags to pass to Haskell compiler |
| `prebuilt_dependencies` | `String list, optional` | Non-Bazel supplied Cabal dependencies |

### haskell_test

This is currently a handy alias for [haskell_binary](#haskell_binary)
so please refer to that for documentation of fields. Additionally, it
accepts
[all common bazel test rule fields](https://docs.bazel.build/versions/master/be/common-definitions.html#common-attributes-tests).
This allows you to influence things like timeout and resource
allocation for the test.

### haskell_doc

Builds API documentation (using [Haddock][haddock]) for the given
Haskell libraries. It will automatically build documentation for any
transitive dependencies to allow for cross-package documentation
linking. Currently linking to `prebuilt_deps` is not supported.

```bzl
haskell_library(
  name = "my-lib",
  …
)

haskell_doc(
  name = "my-lib-doc",
  deps = [":my-lib"],
)
```

[haddock]: http://haskell-haddock.readthedocs.io/en/latest/

#### Attributes

| Attribute | Type | Description |
| --------: | :--- | :---------- |
| `name` | `Name, required` | A unique name for this target |
| `deps` | `Label list, required` | List of Haskell libraries to generate documentation for. |

### haskell_toolchain

Declares a compiler toolchain. You need at least one of these declared
somewhere in your `BUILD` files for the other rules to work. Once
declared, you then need to *register* the toolchain using
`register_toolchain` in your `WORKSPACE` file (see Example below).

```bzl
haskell_toolchain(name, version, tools, ...)
```

Extra arguments forwarded to `toolchain` rule.

#### Example

```bzl
haskell_toolchain(
    name = 'my_ghc',
    version = '1.2.3'
    tools = ["@sys_ghc//:bin"]
)
```

where `@ghc` is an external repository defined in the `WORKSPACE`,
e.g. using:

```bzl
nixpkgs_package(
    name = 'sys_ghc',
    attribute_path = 'haskell.compiler.ghc123'
)

register_toolchain("//:sys_ghc")
```

#### Attributes

| Attribute | Type | Description |
| --------: | :--- | :---------- |
| `name` | `Name, required` | A unique name for this toolchain |
| `version` | `String, required` | Version of the compiler |
| `tools` | `Label list, required` | A target providing GHC commands (`ghc`, `ghc-pkg`, etc) |

### haskell_cc_import

**This rule is temporary replacement for [cc_import][cc_import] and
will be deprecated in the future.**

Imports a prebuilt shared library. Use this to make `.so`, `.dll`,
`.dylib` files residing in
external [external repositories][bazel-ext-repos] available to Haskell
rules.

```bzl
haskell_cc_import(name, shared_library, hdrs)
```

[bazel-ext-repos]: https://docs.bazel.build/versions/master/external.html
[cc_import]: https://docs.bazel.build/versions/master/be/c-cpp.html#cc_import

#### Example

```bzl
haskell_cc_import(name = "zlib", shared_library = "@zlib//:lib")

haskell_binary(
  name = "crc32sum",
  srcs = ["Main.hs"],
  deps = [":zlib"],
  prebuilt_dependencies = ["base"],
)
```

#### Attributes

| Attribute | Type | Description |
| --------: | :--- | :---------- |
| `name` | `Name, required` | A unique name for this toolchain |
| `shared_library` | `Label, required` | Version of the compiler |
| `hdrs` | `Label list, required` | Public headers that ship with the library |

### cc_haskell_import

**This rule is temporary and only needed until the Bazel C/C++
"sandwich" (see [bazelbuild/bazel#2163][bazel-cpp-sandwich]) is
implemented. This rule will be deprecated in the future.**

Given a [haskell_library](#haskell_library) input, it outputs the
shared object file produced as well as the object files it depends on
directly and transitively. This is very useful if you want to link in
a Haskell shared library from `cc_library`.

There is a caveat: this will not provide any shared libraries that
aren't explicitly given to it. This means that if you're using
`prebuilt_dependencies` and relying on GHC to provide those objects,
they will not be present here. You will have to provide those
separately to your `cc_library`. If you're getting
`prebuilt_dependencies` from your toolchain, you will likely want to
extract those and pass them in as well.

[bazel-cpp-sandwich]: https://github.com/bazelbuild/bazel/issues/2163

#### Example

```bzl
haskell_library(
  name = "my-lib",
  …
)

cc_haskell_import(
  name = "my-lib-objects",
  dep = ":my-lib",
)

cc_library(
  name = "my-cc",
  srcs = ["main.c", ":my-lib-objects"],
)
```

#### Attributes

| Attribute | Type | Description |
| --------: | :--- | :---------- |
| `name` | `Name, required` | A unique name for this target |
| `dep` | `Label, required` | Target providing a `HaskellPackageInfo` such as `haskell_library` |

## Language interop

We may be supporting interop with other languages in one way or
another. Please see languages listed below about how.

### Java

You can supply `java_*` rule targets in `deps` of
[haskell_binary](#haskell_binary) and
[haskell_library](#haskell_library). This will make jars produced by
those dependencies available during Haskell source compilation phase
(i.e. not during linking &c. but it's subject to change) and set the
CLASSPATH for that phase as well.
