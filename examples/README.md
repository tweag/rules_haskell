# rule_haskell examples

Examples of using [rules_haskell][rules_haskell], the Bazel rule set
for building Haskell code.

* [**vector:**](./vector/) shows how to build the `vector` package
  that is found on Hackage (as well as transitive dependencies
  `primitive` and `transformers`) without using Cabal.
* [**cat_hs:**](./cat_hs/) is an example of a non-trivial application
  with multiple third-party dependencies downloaded from Hackage,
  C library dependencies and split up into multiple libraries and
  a binary. We use a rule wrapping Cabal to build the Hackage
  dependencies.
* [**rts:**](./rts/) demonstrates foreign exports and shows how to
  link against GHC's RTS library, i.e. `libHSrts.so`.
  
## Root Workspace

Build everything in the root workspace with:

```
$ bazel build //...
```

Show every target of the vector example:

```
$ bazel query //vector/...
//vector:vector
//vector:semigroups
//vector:primitive
//vector:ghc-prim
//vector:deepseq
//vector:base
```

Build the three main Haskell targets:

```
$ bazel build //cat_hs/exec/cat_hs
$ bazel build //vector
$ bazel build //rts:add-one-hs
```

[rules_haskell]: https://github.com/tweag/rules_haskell
