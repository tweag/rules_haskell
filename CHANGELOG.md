# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/).

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
