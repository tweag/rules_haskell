# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/).

## [0.8] - 2019-xx-xx

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

### Fixed

* Fix static linking with C libraries that are indirectly depended upon

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

* Linking issues on MacOS. See
  [#221](https://github.com/tweag/rules_haskell/pull/221).

* GHC packages that correspond to targets with the same name but in
  different Bazel packages no longer clash. See
  [#219](https://github.com/tweag/rules_haskell/issues/219).

* Build breakage on MacOS when XCode is not installed. See
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

* Support for MacOS, courtesy of Judah Jacobson. See
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
