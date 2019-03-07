# cat_hs - A Hazel Example Project

This project re-implements a subset of the `cat` command-line tool in Haskell.
It serves as an example of a project built using the [Bazel][bazel] build
system, using [rules_haskell][rules_haskell] to define the Haskell build, using
[Hazel][hazel] to manage third-party Haskell dependencies, and using
[rules_nixpkgs][rules_nixpkgs] and [Nix][nix] to manage system dependencies.

[bazel]: https://bazel.build/
[rules_haskell]: https://haskell.build/
[hazel]: https://github.com/FormationAI/hazel
[rules_nixpkgs]: https://github.com/tweag/rules_nixpkgs
[nix]: https://nixos.org/nix/

## Prerequisites

You need to install the [Nix package manager][nix]. All further dependencies
will be managed using Nix.

## Instructions

To build the package execute the following command in the checked out source
repository.

```
$ nix-shell --pure --run "bazel build //..."
```

To run the tests execute the following command.

```
$ nix-shell --pure --run "bazel test //..."
```

To run the executable enter the following commands.

```
$ nix-shell --pure --run "bazel run //exec/cat_hs -- -h"
$ nix-shell --pure --run "bazel run //exec/cat_hs -- $PWD/README.md"
```
