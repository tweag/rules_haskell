# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/).

## Upcoming release

### Changed

* The platform name for Nix users has changed. The platforms in
  `@//haskell/platforms/...` are still supported, but are deprecated.
  Use `@io_tweag_rules_nixpkgs//nixpkgs/platforms:host` instead.

### [0.12.0] - 2020-03-16

## Highlights

* Various improvements to Windows support.

* Support for Bazel 2.0.0

* Minimum supported Bazel version is now 0.29, but GHC bindist is known to
  [fail on macOS](https://app.circleci.com/jobs/github/tweag/rules_haskell/8684)
  with Bazel 0.29. Please use a more recent version for macOS.

## Added

* `haskell_repl` now has a `hie_bios` output group
  See [#1263](https://github.com/tweag/rules_haskell/pull/1263)
* Added support for [hrepl](https://github.com/google/hrepl)
  (a standalone binary that runs REPLS for Bazel Haskell targets).
  See [#1210](https://github.com/tweag/rules_haskell/pull/1210).
* `haskell_cabal_library`, `haskell_cabal_binary`, and `stack_snapshot`
  now have a `verbose` argument, to allow suppressing their output.
  See [#1208](https://github.com/tweag/rules_haskell/pull/1208).
* `haskell_cabal_library` and `stack_snapshot` now build the Haddock
  documentation if the parameter `haddock` is set to `True` (the default).
  See [#1102](https://github.com/tweag/rules_haskell/pull/1102) and
  [#1200](https://github.com/tweag/rules_haskell/pull/1200).
* Added support for GHC bindist versions `8.8.3`, `8.8.2`, and `8.8.1`.
* Windows: support for `cabal`
  See [#1133](https://github.com/tweag/rules_haskell/pull/1133).
* rules_haskell now depends on [rules_sh](https://github.com/tweag/rules_sh),
  a toolchain for common shell commands.
  See [#1117](https://github.com/tweag/rules_haskell/pull/1117),
  [#1143](https://github.com/tweag/rules_haskell/pull/1143), and
  [#1136](https://github.com/tweag/rules_haskell/pull/1136) for motivation.
  See [#1096](https://github.com/tweag/rules_haskell/issues/1096)
  for the issue that triggered this train of thought.

## Removed

* `hazel` has been deleted, please use `stack_snapshot` instead.
  See [#1158](https://github.com/tweag/rules_haskell/pull/1158).

## Changed

* The `haskell_register_toolchains()` is no longer defined in
  `haskell/repositories.bzl`, load it from `haskell/toolchain.bzl` instead.
* `cabal` wrapper: specify `python3` as a requirement,
  to enhance error messages on macOS and reduce cache invalidations.
  See [#1251](https://github.com/tweag/rules_haskell/pull/1251),
  [#1097](https://github.com/tweag/rules_haskell/pull/1097), and
  [#1096](https://github.com/tweag/rules_haskell/issues/1096).
* `ghc_bindist` and `haskell_register_ghc_bindists` now have
  a `locale` argument. Set it to circumvent issues on systems without
  the default `C.UTF-8` locale.
  See [#1249](https://github.com/tweag/rules_haskell/pull/1249).
* macOS: `BAZEL_USE_CPP_ONLY_TOOLCHAIN = 1` must be set for Bazel
  to pick the correct C compiler.
  See [#1159](https://github.com/tweag/rules_haskell/pull/1159).
* `stack_snapshot`: warning of stackage dependencies are not shown anymore,
  as they are irrelevant.
  See [#1146](https://github.com/tweag/rules_haskell/pull/1146) and
  [#1026](https://github.com/tweag/rules_haskell/issues/1026).
* Sorted the content of generated manifest files, hereby avoid some spurious
  rebuilds (more builds caching).
  See [#1128](https://github.com/tweag/rules_haskell/pull/1128) and
  [#1126](https://github.com/tweag/rules_haskell/issues/1126).
* Windows: possible race condition on `stack update` is now avoided,
  by calling `stack update` only once.
  See [#1199](https://github.com/tweag/rules_haskell/pull/1199) and
  [#1090](https://github.com/tweag/rules_haskell/issues/1090).

## Fixed

* `haskell_cabal_library` and `haskell_library` now set relative
  `RUNPATH` entries.
  See [#1267](https://github.com/tweag/rules_haskell/pull/1267)
* Fixed that GHC bindist could contain absolute `RUNPATH` to `rts`
  See [#1131](https://github.com/tweag/rules_haskell/pull/1131).
* Documentations of rules `haskelly_library`, `haskell_binary`,
  and `haskell_test` now show the documentation of attributes.
  See [#1122](https://github.com/tweag/rules_haskell/issues/1122).

## [0.11.0] - 2019-10-10

### Highlights

* Various improvements to `cabal_binary/library` and `stack_snapshot`.

* Initial experimental Windows support for `cabal_binary/library` and
  `stack_snapshot`.
  
* [`start`](./start) script for setting up `rules_haskell` allows to set up a nixpkgs-based
  bazel workspace, for NixOS users, via the `--use-nix` option.
  
### Added

* Windows-support for cabal/stack.
  See [#1074](https://github.com/tweag/rules_haskell/pull/1074).
* `stack_snapshot`: `vendored_packages` attribute for manually
  overriding packages in a stack snapshot.
  There is an example in [./examples/WORKSPACE](./examples/WORKSPACE).
  See [#1060](https://github.com/tweag/rules_haskell/pull/1060).
  
### Removed

* The `haskell/haskell.bzl` entrypoint, which was deprecated in the
  previous release, was removed. Please use `haskell/defs.bzl`
  instead.

### Changed

* The `deps` attribute to `stack_snapshot` has been replaced by the
  `extra_deps` attribute. It no longer takes a list of dependencies to be added
  to all packages, but instead a dictionary specifying additional dependencies
  to select packages. See `stack_snapshot` API docs for an example. See
  [#1068](https://github.com/tweag/rules_haskell/pull/1068).
  
### Fixed

* Unified the `cc_wrapper` on all OSes.
  * Consistently shortens paths of library dependencies to work around
    size limits on Windows and macOS
  * fixes `.so/.dylib` ending confusing on macOS
  * improves the REPL on macOS
  See [#1039](https://github.com/tweag/rules_haskell/pull/1039).
* `cabal_binary/library` targets don’t name-clash anymore
* `haskell_cabal_library` no longer builds `exe` components, speeding
  up builds.
  See [#1095](https://github.com/tweag/rules_haskell/pull/1095).
* Haddock information for protobuf rules generates correctly.
  See [#1108](https://github.com/tweag/rules_haskell/pull/1108).

## [0.10.0] - 2019-09-03

### Highlights

* The minimum supported Bazel version is now 0.27.

  `rules_haskell` supports Bazel up to 0.28.
  0.27 is a LTS release, which means upstream guarantees all new
  releases are backwards-compatible to it for 3 months. See the [Bazel
  Stability](https://blog.bazel.build/2019/06/06/Bazel-Semantic-Versioning.html)
  blog post for more information.

* The repository name has changed, to follow the
  new [Bazel rule guidelines][bazel-rule-guidelines]. It was
  previously called `@io_tweag_rules_haskell`. It is now called
  `@rules_haskell`. You should adapt your `WORKSPACE` file to match
  this, as well as your `.bazelrc.local` file, if any.

* `haskell_cabal_library`/`haskell_cabal_binary`: Both use `cabal` to
  build Haskell package dependencies.

* `stack_snapshot`: Uses stack’s dependency resolving algorithm to
  generate `haskell_cabal_library` targets automatically.
  Requires `stack` > 2.1, which it will download if cannot be found in
  `PATH`.

* It is now possible to statically link Haskell libraries in CC
  binaries.

* A new example has been added.

  [**cat_hs:**](./examples/cat_hs/) is an example of a non-trivial application
  with multiple third-party dependencies downloaded from Hackage,
  C library dependencies and split up into multiple libraries and
  a binary. We use a rule wrapping Cabal to build the Hackage
  dependencies. This example requires Nix installed. It is used to
  build (or download from a binary cache) the C library dependencies.

* Improved coverage reports.

* Haddock links to prebuilt libraries.

* Various improvements to reduce header size limits and command line
  argument length, in order to better support macOS and Windows.

[bazel-rule-guidelines]: https://docs.bazel.build/versions/master/skylark/deploying.html

### Added

* `haskell_cabal_library`/`haskell_cabal_binary`.
  See [#879](https://github.com/tweag/rules_haskell/pull/879),
  [#898](https://github.com/tweag/rules_haskell/pull/898) and
  [#904](https://github.com/tweag/rules_haskell/pull/904).
* `stack_snapshot`.
  See [#887](https://github.com/tweag/rules_haskell/pull/887) (name
  changed to `stack_snapshot` after this PR).
  See also [#1011](https://github.com/tweag/rules_haskell/pull/1011).
* `tools` arguments for stack and cabal rules.
  See [#907](https://github.com/tweag/rules_haskell/pull/907).
  They can be arbitrary tools,
  see [#987](https://github.com/tweag/rules_haskell/pull/987).
* `tools` attribute to core `haskell_*` rules. This attribute can be
  used to expose GHC preprocessors.
  See [#911](https://github.com/tweag/rules_haskell/pull/911).
* Static GHC RTS can be specified in the toolchain.
  See [#970](https://github.com/tweag/rules_haskell/pull/970).
* `runfiles` library: manifest file support (for Windows).
  See [#992](https://github.com/tweag/rules_haskell/pull/992).
* Prototype implementation of bazel worker mode (not production-ready
  yet).
  See [#1024](https://github.com/tweag/rules_haskell/pull/1024)
  and [#1055](https://github.com/tweag/rules_haskell/pull/1055)

### Changed

* The `haskell_toolchain` macro now no longer adds a `toolchain`
  definition. You must now define yourself a `haskell_toolchain` and
  a `toolchain` separately. This should be a mostly transparent
  change, because nearly no one uses these functions directly. They
  are normally only used transitively via
  `haskell_register_toolchains` and related functions.
  See [#843](https://github.com/tweag/rules_haskell/pull/843).
* The `haskell/haskell.bzl` entrypoint is deprecated. use
  `haskell/defs.bzl` instead.
* The `haskell_repositories()` macro is deprecated. Use
  `rules_haskell_dependencies()` from `haskell/repositories.bzl`
  instead.
* The `haskell_register_toolchains()` macro is deprecated. Use
  `rules_haskell_toolchains()` from `haskell/repositories.bzl`
  instead.
* The `exports` attribute’s semantics are changed:

  ‘A list of other haskell libraries that will be transparently added
  as a dependency to every downstream rule.’

  The original `exports` is available under the new name
  `reexported_modules`.
  See [#1008](https://github.com/tweag/rules_haskell/pull/1008).
* `@` is allowed in Haskell binary names.
* `haskell_library` may be empty (no files in `srcs`).
  See [#1035](https://github.com/tweag/rules_haskell/pull/1035).

### Removed

* The `haskell_lint` rule has been removed. It should have been
  designed as a test rule and it should have had a different name. The
  rule isn't even necessary for its current purpose: it's more
  convenient to turn on compiler warnings globally in the toolchain
  definition.
* The `cc_haskell_import` and `haskell_cc_import` rules have been removed.
  These rules were redundant since Haskell rules can directly interact with C
  rules. Use the following patterns instead.

  ```
  # To import Haskell from C.
  haskell_library(name = "haskell-lib", ...)
  cc_library(name = "cc-lib", deps = [":haskell-lib"], ...)

  # To import C from Haskell.
  cc_library(name = "cc-lib", ...)
  haskell_library(name = "haskell-lib", deps = [":cc-lib"], ...)

  # To import a pre-built library.
  cc_library(name = "so-lib", srcs = glob(["libxyz.so*", "libxyz.dylib", "libxyz.a", "libxyz.dll"]))
  haskell_library(name = "haskell-lib", deps = [":so-lib"], ...)
  ```

### Fixed

* `haskell_register_ghc_nixpkgs`: Forward all arguments to wrapped
  rules. See [#886](https://github.com/tweag/rules_haskell/pull/886).
  Also support `repository` argument and `nixopts`.
* Haddock links to prebuilt libraries.
  See [#928](https://github.com/tweag/rules_haskell/pull/928)
  and [#934](https://github.com/tweag/rules_haskell/pull/934).
* Documentation for GHC plugin targets is now included in
  the API documentation.
* The Multi-REPL recognizes `haskell_toolchain_library` dependencies.
* Various imrovements to linking. See
  [#930](https://github.com/tweag/rules_haskell/pull/930).
* `$(location)` expansion for “expression is not a declared
  prerequisite of this rule”.
  See [#990](https://github.com/tweag/rules_haskell/pull/990).
* Better error if the compiler version doesn’t match the one specified
  in the toolchain.
  See [#1014](https://github.com/tweag/rules_haskell/pull/1014).
* macOS bindists correctly find `ar` and `sed` invocation was broken.
  See [#1022](https://github.com/tweag/rules_haskell/pull/1022)
  and [#1017](https://github.com/tweag/rules_haskell/pull/1017).
* Allow arbitrary name for `haskell_cabal_library`. See
  [#1034](https://github.com/tweag/rules_haskell/pull/1034).
* Various fixes for `c2hs` on Windows
  See [#1046](https://github.com/tweag/rules_haskell/pull/1046)
  and [#1052](https://github.com/tweag/rules_haskell/pull/1052).
* `:load` command in `ghci`
  See [#1046](https://github.com/tweag/rules_haskell/pull/1046).

### Improved

* Profiling mode (`--compilation_mode="dbg"`).
  See [#896](https://github.com/tweag/rules_haskell/pull/896).
* GHC errors won’t be swallowed anymore.
  See [#1050](https://github.com/tweag/rules_haskell/pull/1050).

## [0.9.1] - 2019-06-03

### Fixed

- Bindists were broken on macOS.
  See [884](https://github.com/tweag/rules_haskell/issues/884).

## [0.9] - 2019-05-07

### Highlights

* The minimum supported Bazel version is now v0.24.

  The version is available from [`nixpkgs`
  unstable](https://github.com/NixOS/nixpkgs/pull/58147) and via
  [official
  releases](https://docs.bazel.build/versions/master/install.html).

* Initial Windows support

  A non-trivial subset of `rules_haskell` is now working on Windows.
  See the [project
  tracker](https://github.com/tweag/rules_haskell/issues?q=is%3Aopen+is%3Aissue+project%3Atweag%2Frules_haskell%2F2)
  for finished and ongoing work.

* Improved OSX support

  Due to the `mach-o` header size limit, we took extra measures to
  make sure generated library paths are as short as possible, so
  linking haskell binaries works even for large dependency graphs.

* Better Bindist support

  The default [`start` script](http://haskell.build/start) sets up a
  bindist-based project by default.
  `rules_nixpkgs` is no longer a required dependency of
  `rules_haskell` (but can still be used as backend).

* Full Haskell–C–Haskell Sandwich

  A `haskell_library` can be now be used nearly anywhere a
  `cc_library` can.

  The old `cc_haskell_import` and `haskell_cc_import` wrapper rules
  are no longer necessary and have been deprecated.

* Greatly improved REPL support

  A new `haskell_repl` rule allows to load multiple source targets by
  source, or compiled, as needed. Example usage:

  ```
  haskell_repl(
    name = "my-repl",
    # Collect all transitive Haskell dependencies from these targets.
    deps = [
        "//package-a:target-1",
        "//package-b:target-2",
    ],
    # Load targets by source that match these patterns.
    include = [
        "//package-a/...",
        "//packaga-b/...",
        "//common/...",
    ],
    # Don't load targets by source that match these patterns.
    exclude = [
        "//package-a/vendored/...",
    ],
  )
  ```

* Support for GHC plugins

  Each `haskell_*` rule now has a `plugins` attribute. It takes a
  list of bazel targets, which should be `haskell_library`s that
  implement the [GHC plugin
  specification](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins).

* Initial Code Coverage support

  Measure coverage of your Haskell code. See the [“Checking Code
  Coverage”](https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#checking-code-coverage)
  section in the manual.

### Compatibility Notice

[`hazel`](https://github.com/FormationAI/hazel) was [merged into
`rules_haskell`](https://github.com/tweag/rules_haskell/pull/733), but
we are not yet certain about the exact interface we want to expose.
`hazel` is therefore not included in this release, and we can’t
guarantee the original, unmerged version is compatible with this
release. If you depend on `hazel`, please use a recent `master` commit
of `rules_haskell`.

### Changed

* `haskell_register_ghc_bindists` is no longer re-exported from
  `//haskell/haskell.bzl`.
  You must now load that macro from `//haskell:nixpkgs.bzl`.

* `rules_nixpkgs` is no longer a dependency of `rules_haskell`.

* `haskell_import` has been renamed to `haskell_toolchain_library`.
  This is a substantial breaking change. But adapting to it should be
  as simple as

  ```
  sed -i 's/^haskell_import/haskell_toolchain_library/' **/BUILD{,.bazel}
  sed -i 's/"haskell_import"/"haskell_toolchain_library"/' **/BUILD{,.bazel}
  ```

* `haskell_toolchain`’s tools attribute is now a list of labels.
  Earlier entries take precendence. To migrate, add `[]` around your
  argument.
  See [#854](https://github.com/tweag/rules_haskell/pull/854).

* The default outputs of `haskell_library` are now the static and/or
  shared library files, not the package database config and cache
  files.

### Added

* `haskell_repl` rule that constructs a ghci wrapper that loads
  multiple targets by source.
  See [#736](https://github.com/tweag/rules_haskell/pull/736).
* `plugins` attribute to `haskell_*` rules to load GHC plugins.
  See [#799](https://github.com/tweag/rules_haskell/pull/799).
* The `HaskellInfo` and `HaskellLibraryInfo` providers are now
  exported and thus accessible by downstream rules.
  See [#844](https://github.com/tweag/rules_haskell/pull/844).
* Generate version macros for preprocessors (`c2hs`, `hsc2hs`).
  See [#847](https://github.com/tweag/rules_haskell/pull/847).
* `bindist_toolchain` rule gets `haddock_flags` and `repl_ghci_args`
  attributes.
* `@repl` targets write json file with build information, usable by
  IDE tools.
  See [#695](https://github.com/tweag/rules_haskell/pull/695).

### Deprecated

* `haskell_cc_import`; use `cc_library` instead.
  See [#831](https://github.com/tweag/rules_haskell/pull/831).
* `cc_haskell_import`; just use `haskell_library` like a `cc_library`.
  See [#831](https://github.com/tweag/rules_haskell/pull/831).

### Fixed

* Support protobuf roots in `haskell_proto_library`.
  See [#722](https://github.com/tweag/rules_haskell/pull/722).
* Made GHC bindist relocatable on *nix.
  See [#853](https://github.com/tweag/rules_haskell/pull/853).
* Various other fixes

## [0.8] - 2019-01-28

* The minimum supported Bazel version is now v0.21.

### Added

* `haskell_register_toolchains`, `haskell_register_ghc_bindists` and
  `haskell_register_ghc_nixpkgs` to register multiple toolchains for
  multiple platforms at once. Toolchains from binary distributions can
  now coexist with toolchains from Nixpkgs, even on the same platform.
  On nixpkgs you need to provide a toolchain. See
  [the `README`](./README.md#Nixpkgs) for instructions.
  See [#597](https://github.com/tweag/rules_haskell/pull/597)
  and [#610](https://github.com/tweag/rules_haskell/pull/610).
* Instructions on how to reference a local checkout of `rules_haskell`.
* `rules_haskell` is forward-compatible with the next breaking changes
  in `bazel` versions, via the `--all_incompatible_changes` flag.
  See [#613](https://github.com/tweag/rules_haskell/pull/613).

### Removed

* The `generate_so` attribute of `haskell_binary` and `haskell_test`
  has been completely superseded by `linkstatic` in the last release
  and became a no-op, so it is removed.
* The `main_file` attribute of `haskell_binary` and `haskell_test`
  had been deprecated because it was a no-op, so it is removed.
* The `prebuilt_dependencies` attribute of all haskell rules
  had been deprecated two versions ago and is removed.
  Use `haskell_import` instead (see docs for usage).
* The `extra_binaries` field is now no longer supported.

### Changed

* `ghc_bindist` now requires a `target` argument. Use
  `haskell_register_ghc_nixpkgs` to call `ghc_bindist` once per known
  target.
  See [#610](https://github.com/tweag/rules_haskell/pull/610).
* `ghc_bindist` now registers itself as a toolchain. We no longer
  require a separate toolchain definition and registration in addition
  to `ghc_bindist`.
  See [#610](https://github.com/tweag/rules_haskell/pull/610).
* `c2hs` support is now provided in a separate toolchain called
  `c2hs_toolchain`, rather than an optional extra to the
  `haskell_toolchain`.
  See [#590](https://github.com/tweag/rules_haskell/pull/590).
* Rename bindist arch names so they are the same as in
  `rules_go/nodejs`.

### Fixed

* Prevent duplicate installs of bazel_skylib
  See [#536](https://github.com/tweag/rules_haskell/pull/536).
* Test suite now executes all binaries, various runtime errors were
  uncovered.
  See [#551](https://github.com/tweag/rules_haskell/pull/551).
* Repl targets that have indirect cc_library dependencies.
  See [#576](https://github.com/tweag/rules_haskell/pull/576).
* `linkstatic` for haskell binaries that have an indirect dependency
  on a prebuilt haskell package.
  See [#569](https://github.com/tweag/rules_haskell/pull/569).
* … and an indirect dependency on a C library.
  See [#567](https://github.com/tweag/rules_haskell/pull/567).
* Prefer linking agains static C libraries with `linkstatic`.
  See [#587](https://github.com/tweag/rules_haskell/pull/587).
* Haddock flags take precedence over GHC compiler flags.
  See [#572](https://github.com/tweag/rules_haskell/pull/572).
* User-defined GHC flags now override default flags.
  See [#607](https://github.com/tweag/rules_haskell/pull/607).
* Dynamic transitive C(++) libraries work.
  See [#627](https://github.com/tweag/rules_haskell/pull/627).

## [0.7] - 2018-12-24

### Added

* Support for Bazel 0.20.0. This is now also the lower bound for the
  supported version.
* Supported reexported modules, via the
  new
  [`exports` attribute](http://api.haskell.build/haskell/haskell.html#haskell_library.exports).
  See [#357](https://github.com/tweag/rules_haskell/issues/357).
* Support `linkstatic` attribute, for building mostly static binaries.
  This is now the default for binaries, to match the C/C++ rules
  defaults.
  See [#378](https://github.com/tweag/rules_haskell/issues/378).
* It is now possible to set default Haddock flags in the toolchain
  definition.
  See [#425](https://github.com/tweag/rules_haskell/pull/425).
* Support wrapping Haskell libraries as shared objects callable from
  Python.
  See [#370](https://github.com/tweag/rules_haskell/issues/370).

### Changed

* REPL targets have changed name. If you have a library target `foo`,
  then the corresponding REPL target is now called `foo@repl`. It was
  previously called `foo-repl`. The old name is still supported but is
  deprecated.
* Don't set a default version number anymore in libraries and
  binaries. Version numbers, and CPP version macros, are now only used
  for packages imported from Hackage. Don't use them otherwise.
  See
  [#386](https://github.com/tweag/rules_haskell/pull/386),
  [#414](https://github.com/tweag/rules_haskell/pull/414)
  and [#446](https://github.com/tweag/rules_haskell/pull/446).
* On macOS, we use `ar` for linking, not Libtool.
  See [#392](https://github.com/tweag/rules_haskell/pull/392).
* The `runfiles` Haskell library has been broken out into a Cabal
  library and published on Hackage.

### Fixed

* Make REPL force building of dependencies.
  See [#363](https://github.com/tweag/rules_haskell/pull/363).
* Don’t crash on inputs missing `.haddock` interface files. See
  [#362](https://github.com/tweag/rules_haskell/pull/362)
* Fix handling of non-unique package names.
  See [#403](https://github.com/tweag/rules_haskell/pull/403).

## [0.6] - 2018-07-21

### Added

* Protocol buffers integration using `proto-lens`. See
  [#239](https://github.com/tweag/rules_haskell/pull/239).

* `strip_include_prefix` attribute to the `haskell_cc_import` rule. See
  [#241](https://github.com/tweag/rules_haskell/pull/241).

* Support for `c2hs` files. See
  [#351](https://github.com/tweag/rules_haskell/pull/351).

* The `extra_srcs` attribute that allows to list non-Haskell source files
  that should be visible during compilation and linking (usually useful with
  TH). See [#292](https://github.com/tweag/rules_haskell/pull/292).

* The `extra_binaries` attribute to the `haskell_toolchain` rule. See
  [#282](https://github.com/tweag/rules_haskell/issues/282).

* A Haskell library for looking up runfiles. See
  [#302](https://github.com/tweag/rules_haskell/pull/302).

* A separate toolchain for `doctest`—`haskell_doctest_toolchain`. See
  [#310](https://github.com/tweag/rules_haskell/pull/310).

* The `compiler_flags` attribute to the `haskell_toolchain` rule allowing to
  specify default compiler flags. See
  [#315](https://github.com/tweag/rules_haskell/issues/315).

* The ability to set locale to be used during compilation by adding the
  `locale` and `locale_archive` attributes to `haskell_toolchain`. See
  [#328](https://github.com/tweag/rules_haskell/pull/328).

* Proper support for profiling. See
  [#332](https://github.com/tweag/rules_haskell/pull/332).

* The `repl_ghci_args` attribute to the `haskell_toolchain` rule. See
  [#334](https://github.com/tweag/rules_haskell/pull/334).

* The `haskell_import` rule allowing us to make specifying dependencies more
  uniform and to deprecate the `prebuilt_dependencies` attribute. See
  [#337](https://github.com/tweag/rules_haskell/pull/337).

### Fixed

* Template Haskell linking against `cc_library`. See
  [#218](https://github.com/tweag/rules_haskell/pull/218).

* Linking issues on macOS. See
  [#221](https://github.com/tweag/rules_haskell/pull/221).

* GHC packages that correspond to targets with the same name but in
  different Bazel packages no longer clash. See
  [#219](https://github.com/tweag/rules_haskell/issues/219).

* Build breakage on macOS when XCode is not installed. See
  [#223](https://github.com/tweag/rules_haskell/pull/223).

* Bug preventing Haddock generation because of missing dynamic shared
  libraries when targets have TH in them. See
  [#226](https://github.com/tweag/rules_haskell/pull/226).

* Hyperlinks between targets contained in different Bazel packages
  (Haddocks). See [#231](https://github.com/tweag/rules_haskell/issues/231).

* Generated source files do not cause issues now. See
  [#211](https://github.com/tweag/rules_haskell/pull/211).

* `data` attributes now allow files in them. See
  [#236](https://github.com/tweag/rules_haskell/issues/236).

* Bug when headers and hsc2hs-produced files were not visible to Haddock.
  See [#254](https://github.com/tweag/rules_haskell/pull/254).

* Bug preventing using genrule-produced headers via `haskell_cc_import`. See
  [#268](https://github.com/tweag/rules_haskell/pull/268).

* Bug that allowed us avoid specifying certain `prebuilt_dependencies` if
  they were already specified for transitive dependencies. See
  [#286](https://github.com/tweag/rules_haskell/issues/286).

* Bug that was making modules generated from `.hsc` and `.chs` files and
  generated modules in general not available in the REPLs. See
  [#323](https://github.com/tweag/rules_haskell/pull/323).

### Changed

* Added `-Wnoncanonical-monad-instances` to default warnings in
  `haskell_lint`.

* How REPLs work. Now there is an optional output per binary/library. Its
  name is the name of target with `-repl` added. Users can then build and
  run such a REPL for any defined target. See
  [#220](https://github.com/tweag/rules_haskell/issues/220) and
  [#225](https://github.com/tweag/rules_haskell/pull/225).

* The `haskell_doc` rule now produces self-contained documentation bundle
  with unified index. See
  [#249](https://github.com/tweag/rules_haskell/pull/249).

* `haskell_lint` now only lints direct dependencies. See
  [#293](https://github.com/tweag/rules_haskell/pull/293).

* `haskell_doctest` has been re-designed. It's now a normal rule that works
  only on direct dependencies and allows to specify modules which should be
  tested, pass custom flags to `doctest` executable. See
  [#342](https://github.com/tweag/rules_haskell/pull/342).

* The `prebuilt_dependencies` attribute of `haskell_binary` and
  `haskell_library` has been deprecated. See
  [#355](https://github.com/tweag/rules_haskell/pull/355).

## [0.5] - 2018-04-15

### Added

* Support for macOS, courtesy of Judah Jacobson. See
  [#165](https://github.com/tweag/rules_haskell/issues/165).

* Support for `data` attributes in `haskell_binary` and `haskell_library`
  rules. See [#167](https://github.com/tweag/rules_haskell/issues/167).

* Output on building of GHC bindists so it's clearer what went wrong in case
  of a failure.

* `haskell_repl` rule allowing to interact with GHCi. See
  [#82](https://github.com/tweag/rules_haskell/issues/82).

* Support for GHC 8.4.1 bindist. See
  [#175](https://github.com/tweag/rules_haskell/issues/175).

* `haskell_lint` rule. See
  [#181](https://github.com/tweag/rules_haskell/issues/181).

* `haskell_doctest` rule. See
  [#194](https://github.com/tweag/rules_haskell/issues/194).

### Changed

* Improved hermeticity of builds. See
  [#180](https://github.com/tweag/rules_haskell/pull/180).

* `cc_haskell_import` now works with `haskell_binary` targets as well. See
  [#179](https://github.com/tweag/rules_haskell/issues/179).

## [0.4] - 2018-02-27

### Added

* `hidden_modules` attribute of the `haskell_library` rule. This allows to
  selectively hide modules in a library. See
  [#152](https://github.com/tweag/rules_haskell/issues/152).

### Fixed

* Test executables now find shared libraries correctly at runtime. See
  [#151](https://github.com/tweag/rules_haskell/issues/151).

* Building of certain modules does not fail with the “file name does not
  match module name” message anymore. See
  [#139](https://github.com/tweag/rules_haskell/issues/139).

* Linking issues that resulted in unresolved symbols due to incorrect order
  in which static libraries are passed to linker are not resolved. See
  [#140](https://github.com/tweag/rules_haskell/issues/140).

* The “grep not found” error is fixed. See
  [#141](https://github.com/tweag/rules_haskell/pull/141).

* System-level shared libraries introduced by `haskell_cc_import` are now
  found correctly during compilation. See
  [#142](https://github.com/tweag/rules_haskell/issues/142).

## [0.3] - 2018-02-13

## [0.2] - 2018-01-07

## [0.1] - 2018-01-02
