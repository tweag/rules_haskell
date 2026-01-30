# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`rules_haskell` provides Bazel build rules for Haskell projects. It extends Bazel with capabilities to build, test, and manage Haskell code at scale, supporting multiple GHC versions and both binary distributions and Nixpkgs-based toolchains.

## Build System

### Core Commands

Build, test, and lint commands:

```bash
# Build all targets
bazel build //...

# Run all tests
bazel test //...

# Build and run tests (separate workspaces)
bazel test //... && cd rules_haskell_tests && bazel test //...

# Format check (buildifier)
bazel run //buildifier && cd rules_haskell_tests && bazel run //buildifier

# Fix formatting
bazel run //buildifier:buildifier-fix && cd rules_haskell_tests && bazel run //buildifier:buildifier-fix

# Build documentation
bazel build //docs:api_html
bazel build //docs:guide_html

# Run coverage
bazel coverage //...
```

### Configuration Management

The repository uses a layered `.bazelrc` configuration:

- `.bazelrc` - imports other configs
- `.bazelrc.common` - shared settings, platform configs
- `.bazelrc.bzlmod` - bzlmod-specific settings
- `.bazelrc.local` - user-specific settings (gitignored)
- `.bazelrc.auth` - authentication for remote cache (gitignored)

Platform-specific configurations:
- `--config=linux-nixpkgs` - Linux with Nixpkgs toolchain
- `--config=macos-nixpkgs` - macOS with Nixpkgs toolchain
- `--config=linux-bindist` - Linux with GHC binary distribution
- `--config=macos-bindist` - macOS with GHC binary distribution
- `--config=windows-bindist` - Windows with GHC binary distribution
- `--config=ci` - CI-specific settings with remote cache

Add your preferred config to `.bazelrc.local`:
```
build --config=linux-nixpkgs
```

### Nix Development Environment

For Nixpkgs-based development (recommended on NixOS, optional elsewhere):

```bash
# Enter nix shell (provides Bazel, GHC, and all dependencies)
nix-shell --pure shell.nix

# Specify GHC version (default: 9.4.8)
nix-shell --pure shell.nix --argstr ghcVersion 9.6.5

# Without doc tools (faster)
nix-shell --pure shell.nix --arg docTools false
```

The nix shell provides: Bazel 6, GHC (version-specific), Stack, Go, Python3, JDK11, git, and various utilities.

### Module Structure

The repository is organized into three separate Bazel workspaces:

1. **`rules_haskell/` (root)** - Core rule definitions and implementation
   - `haskell/*.bzl` - Public rule APIs
   - `haskell/private/*.bzl` - Internal implementation details
   - `haskell/asterius/` - Asterius (Haskell-to-WebAssembly) support
   - `docs/` - Documentation source

2. **`rules_haskell_nix/`** - Nixpkgs integration and examples
   - Tests for Nixpkgs-specific functionality

3. **`rules_haskell_tests/`** - Comprehensive test suite
   - `tests/` - Test cases covering various scenarios
   - Tests must be run separately: `cd rules_haskell_tests && bazel test //...`

### Testing

Tag-based test filtering is critical for platform compatibility:

- `-requires_nix` - Exclude Nixpkgs-only tests when using bindists
- `-dont_test_on_darwin` - Exclude macOS-incompatible tests
- `-dont_test_on_windows` - Exclude Windows-incompatible tests
- `-dont_test_with_bindist` - Exclude tests incompatible with GHC bindists
- `-requires_threaded_rts` - Exclude tests needing threaded runtime
- `coverage-compatible` - Only tests that work with coverage

Tests are automatically filtered based on the selected config (`linux-bindist`, `macos-nixpkgs`, etc.).

### GHC Version Management

GHC versions are managed through:
- `haskell/private/ghc_ci.bzl` - Defines default GHC version
- `haskell/ghc_bindist.bzl` - Binary distribution support
- `stackage_snapshot.yaml` and version-specific variants (`stackage_snapshot_9.4.8.yaml`, etc.)

Supported GHC versions: 9.4.8 (default), 9.6.5, 9.8.2

Set GHC version via environment variable:
```bash
export GHC_VERSION=9.6.5
bazel build //...
```

## Architecture

### Rule Implementation Pattern

Haskell rules follow a three-layer architecture:

1. **Public API** (`haskell/defs.bzl`)
   - User-facing macros and rules
   - Attribute definitions in `_haskell_common_attrs`
   - Exports: `haskell_library`, `haskell_binary`, `haskell_test`, `haskell_doc`, `haskell_repl`

2. **Implementation** (`haskell/private/haskell_impl.bzl`)
   - Core logic for compilation, linking, and packaging
   - Functions: `haskell_library_impl`, `haskell_binary_impl`, `haskell_test_impl`

3. **Actions** (`haskell/private/actions/*.bzl`)
   - Low-level build actions
   - `compile.bzl` - GHC compilation
   - `link.bzl` - Linking executables
   - `package.bzl` - Creating package databases
   - `info.bzl` - Extracting package information

### Provider System

Providers pass information between rules:

- `HaskellLibraryInfo` - Library metadata, exposed modules, dependencies
- `HaskellBinaryInfo` - Binary outputs
- `HaskellModuleInfo` - Experimental module-level information
- `HaskellCoverageInfo` - Code coverage data

### Toolchain Architecture

Toolchains (`haskell/toolchain.bzl`) define platform-specific GHC configurations:
- GHC version and tools (ghc, ghc-pkg, haddock, runghc)
- Compiler flags
- Locale settings
- Platform constraints

Register toolchains via:
- `haskell_register_ghc_bindists()` - Binary distributions
- Nixpkgs-based registration through `rules_nixpkgs`

### Cabal Integration

`stack_snapshot` rule (`haskell/cabal.bzl`) integrates Stackage snapshots:
- Converts Stack's `stackage_snapshot.yaml` to Bazel targets
- Supports custom setup dependencies
- Vendored packages (e.g., custom `ghc-paths` replacement)
- Per-GHC-version snapshot files

Example from WORKSPACE:
```python
stack_snapshot(
    name = "stackage",
    packages = ["base", "bytestring", "lens-family-core", ...],
    local_snapshot = "//:stackage_snapshot_9.4.8.yaml",
    vendored_packages = {
        "ghc-paths": "@rules_haskell//tools/ghc-paths",
    },
)
```

### Aspects for Dependencies

Aspects (`haskell_cc_libraries_aspect`, `haskell_doc_aspect`, `haskell_repl_aspect`) traverse dependency graphs to:
- Collect C/C++ library dependencies
- Generate documentation with transitive deps
- Set up REPL environments

## Development Workflow

### Making Changes

1. **Branch from master** - All development happens on feature branches
2. **Test locally** - Run `bazel test //...` before pushing
3. **Format code** - Run buildifier to format `.bzl` files
4. **Update tests** - Add tests in `rules_haskell_tests/tests/`
5. **Test both workspaces** - Ensure both root and `rules_haskell_tests` pass

### Running Specific Tests

```bash
# Single test target
bazel test //tests:haskell_test_name

# Specific test pattern
bazel test //tests/...:haskell_*

# With specific GHC version
GHC_VERSION=9.6.5 bazel test //tests:some_test

# Force rebuild
bazel test //tests:some_test --nocache_test_results
```

### Debugging

```bash
# Show verbose build output
bazel build //target --verbose_failures

# Show GHC command lines
bazel build //target -s

# Debug sandbox issues
bazel build //target --sandbox_debug

# Profile build
bazel build //target --profile=profile.json
```

### Remote Cache Usage

Remote cache is read-only for developers, read-write for CI. To enable:

1. Create `.bazelrc.auth`:
   ```
   build --remote_header=x-buildbuddy-api-key=YOUR_API_KEY
   ```

2. Add to `.bazelrc.local`:
   ```
   build --config=remote-cache
   ```

### Common Issues and Solutions

**GHC settings file not found with packages using GHC API (doctest, ghcide, proto-lens-protoc)**:
- Use `@rules_haskell//tools/ghc-paths` instead of the `ghc-paths` package
- This prevents baked-in paths to Bazel sandbox directories

**Non-deterministic library ID bug**:
- Known GHC issue causing broken package databases
- See: https://nixos.org/nixpkgs/manual/#how-to-recover-from-ghcs-infamous-non-deterministic-library-id-bug

**Warning about home modules during non-sandboxed builds**:
- Enable sandboxing (default on Linux/macOS)
- Or separate source files into different directories per target

**Windows: protoc.exe missing DLL**:
- Use `--proto_compiler @rules_haskell//tests:protoc` flag
- GHC >= 9.4 uses clang requiring libc++ DLL from `mingw\bin`

## Documentation

Reference documentation is auto-generated using Stardoc:
- Source: Rule docstrings in `.bzl` files
- Output: `docs/api_html` and `docs/guide_html` targets
- Public API reference: https://api.haskell.build
- Tutorial and guide: https://rules-haskell.readthedocs.io/

Local docs preview:
```bash
./serve-docs.sh
```

## Bazel Module (bzlmod) Support

The repository supports both WORKSPACE and MODULE.bazel (bzlmod):

- Run tests with bzlmod: `bazel test //... --enable_bzlmod=true`
- Some GHC versions have limited bzlmod support (9.8.2, 9.6.5)
- Windows uses separate remote cache for bzlmod due to absolute path issues

## CI Configuration

GitHub Actions workflow (`.github/workflows/workflow.yaml`) tests:

- **Matrix dimensions**: OS (Ubuntu, macOS, Windows), module (rules_haskell, rules_haskell_tests), GHC version, bzlmod mode
- **Nixpkgs tests**: Ubuntu 24.04, macOS 14 with nix-shell
- **Bindist tests**: All three OS with binary GHC distributions
- **Examples tests**: Separate workspace with all examples

CI-specific behavior:
- `cache-version` env var to invalidate GitHub Actions cache
- BuildBuddy remote cache with read-write access
- Memory management: `bazel shutdown` between test phases on some platforms

## Project-Specific Notes

- **No Bitnami images** - User configuration forbids Bitnami container images
- **Maintainer**: Aleksei Sviridkin (f@lex.la)
- **Primary branch**: `master` (not `main`)
- **Start script**: `curl https://haskell.build/start | sh` generates initial project setup
- **Platform constraints** defined in `haskell/platforms/list.bzl`
