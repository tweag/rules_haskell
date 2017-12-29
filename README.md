# Haskell rules for [Bazel][bazel]

[![CircleCI](https://circleci.com/gh/tweag/rules_haskell.svg?style=svg)](https://circleci.com/gh/tweag/rules_haskell)

To use these rules, you'll need [Bazel >= 0.8.1][bazel-install]. To
run tests, you'll furthermore need [Nix][nix] installed.

[bazel]: https://bazel.build/
[bazel-install]: https://docs.bazel.build/versions/master/install.html
[nix]: https://nixos.org/nix

## Rules

* [haskell_binary](#haskell_binary)
* [haskell_library](#haskell_library)
* [haskell_toolchain](#haskell_import)
* [haskell_import](#haskell_import)
* [haskell_haddock](#haskell_haddock)

## Setup

Add the following to your `WORKSPACE` file, and select a `$COMMIT` accordingly.

```bzl
http_archive(
    name = "io_tweag_rules_haskell",
    strip_prefix = "rules_haskell-$COMMIT",
    urls = ["https://github.com/tweag/rules_haskell/archive/$COMMIT.tar.gz"],
)
```

and this to your BUILD files.

```bzl
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_binary",
  "haskell_library",
  "haskell_toolchain",
  "haskell_import",
)
```

## Rules

### haskell_binary

Generates a Haskell binary.

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

<table class="table table-condensed table-bordered table-params">
  <colgroup>
    <col class="col-param" />
    <col class="param-description" />
  </colgroup>
  <thead>
    <tr>
      <th colspan="2">Attributes</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>name</code></td>
      <td>
        <p><code>Name, required</code></p>
        <p>A unique name for this target</p>
      </td>
    </tr>
    <tr>
      <td><code>srcs</code></td>
      <td>
        <p><code>List of labels, required</code></p>
        <p>List of Haskell <code>.hs</code> source files used to build the binary</p>
      </td>
    </tr>
    <tr>
      <td><code>deps</code></td>
      <td>
        <p><code>List of labels, required</code></p>
        <p>List of other Haskell libraries to be linked to this target</p>
      </td>
    </tr>
  </tbody>
</table>

### haskell_library

Generates a Haskell library.

```bzl
haskell_library(name, srcs, deps)
```

#### Example

```bzl
haskell_library(
    name = 'hello_lib',
    srcs = glob(['hello_lib/**/*.hs']),
    deps = ["//hello_sublib:lib"]
)
```

<table class="table table-condensed table-bordered table-params">
  <colgroup>
    <col class="col-param" />
    <col class="param-description" />
  </colgroup>
  <thead>
    <tr>
      <th colspan="2">Attributes</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>name</code></td>
      <td>
        <p><code>Name, required</code></p>
        <p>A unique name for this target</p>
      </td>
    </tr>
    <tr>
      <td><code>srcs</code></td>
      <td>
        <p><code>List of labels, required</code></p>
        <p>List of Haskell <code>.hs</code> source files used to build the library</p>
      </td>
    </tr>
    <tr>
      <td><code>deps</code></td>
      <td>
        <p><code>List of labels, required</code></p>
        <p>List of other Haskell libraries to be linked to this target</p>
      </td>
    </tr>
  </tbody>
</table>

### haskell_toolchain

Generates a Haskell library.

```bzl
haskell_toolchain(name, version, tools, ...)
```

Extra arguments forwarded to `toolchain` rule.

#### Example

```bzl
haskell_toolchain(
    name = 'ghc',
    version = '1.2.3'
    tools = ["@ghc//:bin"]
)
```

where `@ghc` is an external repository defined in the `WORKSPACE`,
e.g. using:

```bzl
nixpkgs_package(
    name = 'ghc',
    attribute_path = 'haskell.compiler.ghc123'
)
```

<table class="table table-condensed table-bordered table-params">
  <colgroup>
    <col class="col-param" />
    <col class="param-description" />
  </colgroup>
  <thead>
    <tr>
      <th colspan="2">Attributes</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>name</code></td>
      <td>
        <p><code>Name, required</code></p>
        <p>A unique name for this toolchain</p>
      </td>
    </tr>
    <tr>
      <td><code>version</code></td>
      <td>
        <p><code>String, required</code></p>
        <p>Version of the compiler.</p>
      </td>
    </tr>
    <tr>
      <td><code>tools</code></td>
      <td>
        <p><code>Label, required</code></p>
        <p>A target providing GHC commands (`ghc`, `ghc-pkg`, etc).</p>
      </td>
    </tr>
  </tbody>
</table>

### haskell_import

Imports a prebuilt shared library. Use this to make `.so`, `.dll`,
`.dylib` files residing in
external [external repositories][bazel-ext-repos] available to Haskell
rules.

```bzl
haskell_import(name, shared_library, visibility = None)
```

[bazel-ext-repos]: https://docs.bazel.build/versions/master/external.html

#### Example

```bzl
haskell_import(name = "zlib", shared_library = "@zlib//:lib")

haskell_binary(
  name = "crc32sum",
  srcs = ["Main.hs"],
  deps = [":zlib"],
  prebuilt_dependencies = ["base"],
)
```

<table class="table table-condensed table-bordered table-params">
  <colgroup>
    <col class="col-param" />
    <col class="param-description" />
  </colgroup>
  <thead>
    <tr>
      <th colspan="2">Attributes</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>name</code></td>
      <td>
        <p><code>Name, required</code></p>
        <p>A unique name for this target</p>
      </td>
    </tr>
    <tr>
      <td><code>shared_library</code></td>
      <td>
        <p><code>Label, required</code></p>
        <p>A single precompiled shared library.</p>
      </td>
    </tr>
  </tbody>
</table>

### haskell_haddock

Builds [Haddock](http://haskell-haddock.readthedocs.io/en/latest/)
documentation for given Haskell libraries. It will automatically
build documentation for any transitive dependencies to allow for
cross-package documentation linking. Currently linking to
`prebuilt_deps` is not supported.

```bzl
haskell_library(
  name = "my-lib",
  …
)

haskell_haddock(
  name = "my-lib-haddock",
  deps = [":my-lib"],
)
```

<table class="table table-condensed table-bordered table-params">
  <colgroup>
    <col class="col-param" />
    <col class="param-description" />
  </colgroup>
  <thead>
    <tr>
      <th colspan="2">Attributes</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>name</code></td>
      <td>
        <p><code>Name, required</code></p>
        <p>A unique name for this target</p>
      </td>
    </tr>
    <tr>
      <td><code>deps</code></td>
      <td>
        <p><code>List of labels, required</code></p>
        <p>List of Haskell libraries to generate documentation for.</p>
      </td>
    </tr>
  </tbody>
</table>
