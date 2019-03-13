# rule_haskell examples

Examples of using [rules_haskell][rules_haskell], the Bazel rule set
for building Haskell code.

* [**vector:**](./vector/) shows how to build the `vector` package as
  found on Hackage, using a Nix provided compiler toolchain.
* [**rts:**](./rts/) demonstrates foreign exports and shows how to
  link against GHC's RTS library, i.e. `libHSrts.so`.
  
## **Important**

Run all commands from the root of `rules_haskell`.
If you `cd examples/`, bazel *will* [break on
you](https://github.com/tweag/rules_haskell/issues/740).
This is a current problem with bazel workspaces.

## Root Workspace

Build everything in the root workspace with;

```
$ bazel build @io_tweag_rules_haskell_examples//...
```

Show every target of the vector example;

```
$ bazel query @io_tweag_rules_haskell_examples//vector/...
@io_tweag_rules_haskell_examples//vector:vector
@io_tweag_rules_haskell_examples//vector:semigroups
@io_tweag_rules_haskell_examples//vector:primitive
@io_tweag_rules_haskell_examples//vector:ghc-prim
@io_tweag_rules_haskell_examples//vector:deepseq
@io_tweag_rules_haskell_examples//vector:base
```

Build the two main Haskell targets;

```
$ bazel build @io_tweag_rules_haskell_examples//vector
$ bazel build @io_tweag_rules_haskell_examples//rts:add-one-hs
```

## Tutorial Workspace

Build everything in the tutorial workspace with;

```
$ bazel build @tutorial//...
```

Show everything in the tutorial;

```
$ bazel query @tutorial//...
@tutorial//main:demorgan
@tutorial//main:base
@tutorial//lib:booleans
@tutorial//:ghc-impl
@tutorial//:ghc
```

Build and run the tutorial example;

```
$ bazel build @tutorial//lib:booleans
$ bazel build @tutorial//main:demorgan
$ bazel run @tutorial//main:demorgan
```

[rules_haskell]: https://github.com/tweag/rules_haskell
[tutorial]: https://rules-haskell.readthedocs.io
[hazel]: https://github.com/FormationAI/hazel
