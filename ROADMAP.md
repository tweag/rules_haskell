# Feature roadmap

In the following list, each feature is associated with a corresponding
milestone. The convention for the priorities are:

* P0 feature will block the milestone; we will delay the milestone
  date until the feature is shipped.
* P1 feature can delay the milestone if the feature can be shipped
  with a reasonable delay.
* P2 feature will be dropped and rescheduled for later rather than
  delaying the milestone.

We will update this list when reaching each milestone. Some milestones
may also be refined if appropriate.

## Planned feature list

### Build and test inline-java

### Build and test sparkle

### Build and test Agda

### 1.0

* P1. Maximally incremental rebuilds.
* P1. Import toolchains from `rules_nixpkgs`.
* P2. Define official GHC bindists as toolchains for each Tier-1
  platform.
* P2. Define cross-compiler toolchains.
* P2. Support multiple build flavours: fastbuild, opt, dbg/profiling.
* P2. Support passing compiler flags on command-line.

## Previous milestones

### Initial support

* P0. Ensure legalese is in place from the beginning to make project
  upstreamable to official `bazelbuild` org eventually.
* P0. `haskell_library` able to compile single file library.
* P0. `haskell_binary` able to compile single file binary.
* P1. Basic binary build with a library dependency.
* P2. Transitive library dependencies.
* P2. Basic documentation with rule descriptions.
