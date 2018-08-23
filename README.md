# rule_haskell examples

Examples of using [rules_haskell][rules_haskell], the Bazel rule set
for building Haskell code.

* [**vector:**](./vector/) shows how to build the `vector` package as
  found on Hackage, using a Nix provided compiler toolchain.
* [**rts:**](./rts/) demonstrates foreign exports and shows how to
  link against GHC's RTS library, i.e. `libHSrts.so`.
* [**tutorial:**](./tutorial/) a separate workspace for the [tutorial](tutorial).

## Root Workspace

Build everything in the root workspace with;

```
$ bazel build //...
```

Show every target in this workspace;

```
$ bazel query //...
//vector:vector
//vector:semigroups
//vector:primitive
//vector:ghc-prim
//vector:deepseq
//vector:base
//rts:add-one
//rts:add-one-so
//rts:add-one-hs
//rts:base
//:ghc-impl
//:ghc
```

Build the two main Haskell targets;

```
$ bazel build //vector:vector
$ bazel build //rts:add-one-hs
```

## Tutorial Workspace

Build everything in the tutorial workspace with;

```
> bazel build @tutorial//...
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
