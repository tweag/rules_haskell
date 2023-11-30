<p align="left"><img src="logo/horizontal.png" alt="rules_haskell" height="100px"></p>

# Haskell rules for [Bazel][bazel]

[![Continuous Integration](https://github.com/tweag/rules_haskell/actions/workflows/workflow.yaml/badge.svg?event=schedule)](https://github.com/tweag/rules_haskell/actions/workflows/workflow.yaml)

Bazel CI: [![Build status](https://badge.buildkite.com/1de3270f1df070a99978adb6efa180d0c32eac58e0fd1938d1.svg?branch=master)](https://buildkite.com/bazel/rules-haskell-haskell)

Bazel automates building and testing software. It scales to very large
multi-language projects. This project extends Bazel with build rules
for Haskell. Get started building your own project using these rules
with the [setup script below](#setup).

[bazel]: https://bazel.build/
[bazel-getting-started]: https://bazel.build/start
[bazel-cli]: https://docs.bazel.build/versions/master/command-line-reference.html
[external-repositories]: https://docs.bazel.build/versions/master/external.html
[nix]: https://nixos.org/nix

## Rule summary

The full reference documentation for rules is at https://haskell.build.

## Setup

You'll need [Bazel >= 6.0][bazel-getting-started] installed.

If you are on NixOS, skip to the [Nixpkgs](#Nixpkgs) section.

### System dependencies

Refer to the "Before you begin" section in [the documentation](docs/haskell.rst).

### The easy way

In a fresh directory, run:

```console
$ curl https://haskell.build/start | sh
```

Alternatively, if you want to start a project with bzlmod, run:

```console
$ sh <(curl https://haskell.build/start) --with-bzlmod=true
```

This will generate initial `WORKSPACE` and `BUILD` files for you. See the
[examples](./examples) and the [API reference](#Rules) below to adapt these for
your project. Then,

```console
$ bazel build //...    # Build all targets
$ bazel test //...     # Run all tests
```

You can learn more about Bazel's command line
syntax [here][bazel-cli]. Common [commands][bazel-cli-commands] are
`build`, `test`, `run` and `coverage`.


### Nixpkgs

This rule set supports using [Nixpkgs][nixpkgs] to provision your GHC
toolchain and to fetch hackage packages from there. To create your
project, pass `--use-nix`, like so:

```console
$ sh <(curl https://haskell.build/start) --use-nix
```

This generates the same files as above, but uses `nixpkgs` to
provision GHC.

If you are on NixOS, this is the only way to set up your project,
because the GHC toolchain provisioned through binary distributions
cannot be executed on NixOS.

If you are on macOS, you will have to set the environment variable
`BAZEL_USE_CPP_ONLY_TOOLCHAIN = 1`, so that Bazel picks the correct C compiler.

[bazel-cli-commands]: https://docs.bazel.build/versions/master/command-line-reference.html#commands
[nixpkgs]: https://nixos.org/nixpkgs/

## Tutorial and Examples

We provide a [tutorial for writing your first rules][tutorial].
The corresponding source code is in [./tutorial](./tutorial).

A collection of example rules is in [./examples](./examples).

[tutorial]: https://rules-haskell.readthedocs.io/en/latest/

## Rules

See https://api.haskell.build for the reference documentation on provided
rules. Using [./serve-docs.sh](./serve-docs.sh), you can also view
this documentation locally.

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
    tools = ["@ghc//:bin"],
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
    tools = ["@ghc//:bin"],
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

### Warning about home modules during non-sandboxed builds

Say you have a folder that mixes source files for two different
libraries or for a library and an executable. If you build with
sandboxing turned off, it is possible that GHC will use the source
files for one library during the build of the other. The danger in
this situation is that because GHC used inputs that Bazel didn't know
about, incremental rebuilds might not be correct. This is why you get
a warning of the following form if this happens:

```
<no location info>: warning: [-Wmissing-home-modules]
    Modules are not listed in command line but needed for compilation: Foo
```

Turning sandboxing on (this is Bazel's default on Linux and macOS)
protects against this problem. If sandboxing is not an option, simply
put the source files for each target in a separate directory (you can
still use a single `BUILD` file to define all targets).

### hGetContents: invalid argument (invalid byte sequence)

If you are using the GHC bindists and see an error message like this:

```
haddock: internal error: /tmp/tmputn68mya/doc/html/path-io/haddock-response300-1.txt: hGetContents: invalid argument (invalid byte sequence)
```

It means that the default locale (`C.UTF-8`) does not work on your system.
You can use a locale that your system has. For example, if your system has the
locale `en_US.UTF-8`, you can specify that locale:

```bzl
rules_haskell_toolchains(
    locale = "en_US.UTF-8", # <----
    version = "8.4.1",
)
```

To find available locales, run `locale -a` in a terminal. You should see output like the following:

```console
$ locale -a
C
en_US
en_US.iso88591
en_US.utf8
POSIX
```

### MacOS: Error: DEVELOPER_DIR not set.

Make sure to set the following environment variable:
```
export BAZEL_USE_CPP_ONLY_TOOLCHAIN=1
```
This ensures that Bazel picks the correct C compiler.

### Windows: Incorrect `cc_toolchain` used

If you're using Windows, bazel might use a different `cc_toolchain`
than is required to build. This might happen if the environment has a
`cc_toolchain` from Visual Studio. This might show up with an error like:
```
Traceback (most recent call last):
  File "\\?\C:\Users\appveyor\AppData\Local\Temp\1\Bazel.runfiles_w5rfpqk5\runfiles\rules_haskell\haskell\cabal_wrapper.py", line 105, in <module>
    strip = find_exe("external/local_config_cc/wrapper/bin/msvc_nop.bat")
  File "\\?\C:\Users\appveyor\AppData\Local\Temp\1\Bazel.runfiles_w5rfpqk5\runfiles\rules_haskell\haskell\cabal_wrapper.py", line 56, in find_exe
    if not os.path.isfile(path) and "True" == "True":
  File "C:\Python37\lib\genericpath.py", line 30, in isfile
    st = os.stat(path)
TypeError: stat: path should be string, bytes, os.PathLike or integer, not NoneType
```

You can override the `cc_toolchain` chosen with the following flag:
```
--crosstool_top=@rules_haskell_ghc_windows_amd64//:cc_toolchain
```
This chooses the `cc_toolchain` bundled with GHC.

### GHC settings file does not exist

If you use the GHC bindist toolchain, i.e. `haskell_register_ghc_bindists`, then you may encounter the following type of error with packages that use the GHC API, e.g. `doctest`, `ghcide`, or `proto-lens-protoc`:

```
.../lib/settings: openFile: does not exist (No such file or directory)
```

This could be caused by a dependency on the `ghc-paths` package which bakes the path to the GHC installation at build time into the binary. In the GHC bindist use case this path points into Bazel's sandbox working directory which can change between build actions and between build-time and runtime.

You can use `@rules_haskell//tools/ghc-paths` as a drop-in replacement to work around this issue. See `tools/ghc-paths/README.md` for further details.

### Windows: protoc.exe exits with an error

If you see
```
protoc.exe: error while loading shared libraries: api-ms-win-crt-filesystem-l1-1-0.dll: cannot open shared object file: No such file or directory
```
or
```
Process finished with exit code -1073741515 (0xC0000135)
```
this usually means the executable cannot find a DLL it depends on (not necessarily the DLL that is mentioned in the error message).

Newer Windows GHC distributions (>= 9.4), come with clang as the C/C++ compiler, and executables produced using that toolchain depend on the libc++ DLL, which is found in the `mingw\bin` directory of the bindist. You can pass `--proto_compiler @rules_haskell//tests:protoc` as a build flag to bazel as a workaround (see [tests/protoc.bzl]).

## For `rules_haskell` developers

### Configuring your platform

`rules_haskell` can be built and tested on Linux, MacOS, and Windows. Depending
on the platform GHC can be provisioned using nixpkgs or by downloading a binary
distribution. In case of nixpkgs other toolchains (C compiler, Python, shell
tools) will also be provided by nixpkgs, in case of bindist they will be taken
from the environment (`$PATH`). The following `--config` options select the
corresponding combination of operating system and GHC distribution:

|                     |      Linux      |      MacOS      |      Windows      |
| ------------------- | --------------- | --------------- | ----------------- |
| nixpkgs             | `linux-nixpkgs` | `macos-nixpkgs` |                   |
| binary distribution | `linux-bindist` | `macos-bindist` | `windows-bindist` |

Hint: You can use Bazel's `--announce_rc` flag to see what options are being
used for a command in a specific configuration. E.g.
```
$ bazel build //tests:run-tests --config linux-nixpkgs --nobuild --announce_rc
```

Hint: To avoid repetition you can add your configuration to `.bazelrc.local`.
```
echo "build --config=linux-nixpkgs" >>.bazelrc.local
```

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
    name = "rules_haskell",
    strip_prefix = "rules_haskell-" + version,
    sha256 = …,
    urls = …,
)
```

To reference a local checkout instead, use the
[`--override_repository`][override_repository] command line option:

```
bazel build/test/run/sync \
  --override_repository rules_haskell=/path/to/checkout
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
$ bazel test //... && cd rules_haskell_tests && bazel test //...
```

Starlark code in this project is formatted according to the output of
[buildifier]. You can check that the formatting is correct using:

```
$ bazel run //buildifier && cd rules_haskell_tests && bazel run //buildifier
```

If tests fail then run the following to fix the formatting:

```
$ git rebase --exec "bazel run //buildifier:buildifier-fix && cd rules_haskell_tests && bazel run //buildifier:buildifier-fix" <first commit>
```

where `<first commit>` is the first commit in your pull request.
This fixes formatting for each of your commits separately, to keep
the history clean.

[buildifier]: https://github.com/bazelbuild/buildtools/tree/master/buildifier

### <a name="nixpkgs-pin" />How to update the nixpkgs pin

You have to find a new git commit where all our `shell.nix`
dependencies are available from the official NixOS Hydra binary cache.

At least for `x86-linux` this is guaranteed for the `unstable`
channels. You can find the `nixpkgs` git commit of current `unstable`
here:

https://nixos.org/channels/nixos-unstable/git-revision

That might be too old for your use-case (because all tests have to
pass for that channel to be updated), so as a fallback there is:

https://nixos.org/channels/nixos-unstable-small/git-revision

You copy that hash to `url` in
[`./nixpkgs/default.nix`](./nixpkgs/default.nix). Don’t forget to
change the `sha256` or it will use the old version. Please update the
date comment to the date of the `nixpkgs` commit you are pinning to.

### GitHub Actions Cache

The GitHub actions CI pipeline uses
[`actions/cache`](https://github.com/actions/cache) to store the Bazel
repository cache. The `cache-version` must be updated manually in the `env`
section in the [workflow](./.github/workflows/workflow.yaml) to invalidate the
cache if any cacheable external dependencies are changed.

#### “unable to start any build”

```
error: unable to start any build; either increase '--max-jobs' or enable remote builds
```

We set `--builders ""` and `--max-jobs 0` on CI to be sure all
dependencies are coming from binary caches. You might need to add an
exception (TODO: where to add exception) or [switch to a different
nixpkgs pin](#nixpkgs-pin).
