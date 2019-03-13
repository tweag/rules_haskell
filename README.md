<p align="left"><img src="logo/horizontal.png" alt="rules_haskell" height="100px"></p>

# Haskell rules for [Bazel][bazel]

[![CircleCI](https://circleci.com/gh/tweag/rules_haskell.svg?style=svg)](https://circleci.com/gh/tweag/rules_haskell)
[![Build Status](https://dev.azure.com/tweag/rules_haskell/_apis/build/status/tweag.rules_haskell?branchName=master)](https://dev.azure.com/tweag/rules_haskell/_build/latest?definitionId=1?branchName=master)

Bazel automates building and testing software. It scales to very large
multi-language projects. This project extends Bazel with build rules
for Haskell. Get started building your own project using these rules
wih the [setup script below](#setup).

[bazel]: https://bazel.build/
[bazel-getting-started]: https://docs.bazel.build/versions/master/getting-started.html
[bazel-cli]: https://docs.bazel.build/versions/master/command-line-reference.html
[external-repositories]: https://docs.bazel.build/versions/master/external.html
[nix]: https://nixos.org/nix

## Rule summary

The full reference documentation for rules is at https://haskell.build.

## Setup

You'll need [Bazel >= 0.22.0][bazel-getting-started] installed.

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

### Nixpkgs

This rule set supports [Nixpkgs][nixpkgs]. If you are on NixOS, or if
you are using Nixpkgs on your project, consider passing the following
argument on the command-line to select a Nixpkgs-based toolchain for
the build:

```
$ bazel build --host_platform=@io_tweag_rules_haskell//haskell/platforms:linux_x86_64_nixpkgs //...
```

See [below](#saving-common-command-line-flags-to-a-file) to
permanently set that flag.

[bazel-cli-commands]: https://docs.bazel.build/versions/master/command-line-reference.html#commands
[nixpkgs]: https://nixos.org/nixpkgs/

### Doing it manually

Add the following to your `WORKSPACE` file, and select a `$VERSION`
(or even an arbitrary commit hash) accordingly.

```bzl
http_archive(
  name = "io_tweag_rules_haskell",
  strip_prefix = "rules_haskell-$VERSION",
  urls = ["https://github.com/tweag/rules_haskell/archive/v$VERSION.tar.gz"],
)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
	"haskell_repositories",
	"haskell_register_toolchains",
)

haskell_repositories()

haskell_register_toolchains()
```

You will then need to write one `BUILD` file for each "package" you
want to define. See below for examples.

## Tutorial and Examples

We provide a [tutorial for writing your first rules][tutorial].
The corresponding source code is in [./tutorial](./tutorial).

A collection of example rules is in [./examples](./examples).

[tutorial]: https://rules-haskell.readthedocs.io/en/latest/

## For `rules_haskell` developers

### Saving common command-line flags to a file

If you find yourself constantly passing the same flags on the
command-line for certain commands (such as `--host_platform` or
`--compiler`), you can augment the [`.bazelrc`](./.bazelrc) file in
this repository with a `.bazelrc.local` file. This file is ignored by
Git.

### Reference a local checkout of `rules_haskell`

When you develop on `rules_haskell`, you usually do it in the context
of a different project that has `rules_haskell` as a `WORKSPACE`
dependency, like so:

```
http_archive(
    name = "io_tweag_rules_haskell",
    strip_prefix = "rules_haskell-" + version,
    sha256 = …,
    urls = …,
)
```

To reference a local checkout instead, use the
[`--override_repository`][override_repository] command line option:
   
```
bazel build/test/run/sync \
  --override_repository io_tweag_rules_haskell=/path/to/checkout
```
   
If you don’t want to type that every time, [temporarily add it to
`.bazelrc`][bazelrc].

[override_repository]: https://docs.bazel.build/versions/master/command-line-reference.html#flag--override_repository
[local_repository]: https://docs.bazel.build/versions/master/be/workspace.html#local_repository
[bazelrc]: https://docs.bazel.build/versions/master/best-practices.html#bazelrc

### Test Suite

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
$ bazel run //:buildifier
```

If tests fail then run the following to fix the formatting:

```
$ bazel run //:buildifier-fix
```

[buildifier]: https://github.com/bazelbuild/buildtools/tree/master/buildifier

### CircleCI

Pull Requests are checked by CircleCI.

If a check fails and you cannot reproduce it locally (e.g. it failed on Darwin
and you only run Linux), you can [ssh into CircleCI to aid debugging][ci-ssh].

[ci-ssh]: https://circleci.com/docs/2.0/ssh-access-jobs/

## Rules

See https://api.haskell.build for the reference documentation on provided
rules. Using [./serve-docs.sh](./serve-docs.sh), you can also view
this documentation locally.

## Language interop

We may be supporting interop with other languages in one way or
another. Please see languages listed below about how.

### C/C++

C/C++ libraries can be specified as dependencies. Exporting Haskell libraries
as C/C++ dependencies currently requires the `cc_haskell_import` rule. This is
a temporary workaround to Bazel limitations.

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

## Troubleshooting

### No such file or directory

If you see error messages complaining about missing `as` (`ld` or indeed
some other executable):

```
cc: error trying to exec 'as': execvp: No such file or directory
`cc' failed in phase `Assembler'. (Exit code: 1)
```

It means that your `gcc` cannot find `as` by itself. This happens only on
certain operating systems which have `gcc` compiled without `--with-as` and
`--with-ld` flags. We need to make `as` visible manually in that case:

```bzl
# Create a symlink to system executable 'as'
genrule(
    name = "toolchain_as",
    outs = ["as"],
    cmd = "ln -s /usr/bin/as $@",
)

# Make it visible to rules_haskell rules:
haskell_toolchain(
    name = "ghc",
    tools = "@ghc//:bin",
    version = "8.4.1",
    extra_binaries = [":toolchain_as"], # <----
)
```

### `__STDC_VERSION__` does not advertise C99 or later

If you see an error message like this:

```
/root/.cache/bazel/_bazel_root/b8b1b1d6144a88c698a010767d2217af/external/ghc/lib/ghc-8.4.1/include/Stg.h:29:3: error:
     error: #error __STDC_VERSION__ does not advertise C99 or later
     # error __STDC_VERSION__ does not advertise C99 or later
       ^
   |
29 | # error __STDC_VERSION__ does not advertise C99 or later
   |   ^
```

It means that your `gcc` selects incorrect flavor of C by default. We need
C99 or later, as the error message says, so try this:

```bzl
haskell_toolchain(
    name = "ghc",
    tools = "@ghc//:bin",
    version = "8.4.1",
    compiler_flags = ["-optc-std=c99"], # <----
)
```

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
