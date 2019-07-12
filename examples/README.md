# rule_haskell examples

Examples of using [rules_haskell][rules_haskell], the Bazel rule set
for building Haskell code.

* [**vector:**](./vector/) shows how to build the `vector` package as
  found on Hackage, using a Nix provided compiler toolchain.
* [**rts:**](./rts/) demonstrates foreign exports and shows how to
  link against GHC's RTS library, i.e. `libHSrts.so`.
  
## Root Workspace

Build everything in the root workspace with;

```
$ bazel build //...
```

Show every target of the vector example;

```
$ bazel query //vector/...
//vector:vector
//vector:semigroups
//vector:primitive
//vector:ghc-prim
//vector:deepseq
//vector:base
```

Build the two main Haskell targets;

```
$ bazel build //vector
$ bazel build //rts:add-one-hs
```

[rules_haskell]: https://github.com/tweag/rules_haskell
