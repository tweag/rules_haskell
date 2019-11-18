# cat_hs - A rules_haskell Example Project

This project re-implements a subset of the `cat` command-line tool in
Haskell. It serves as an example of a project built using
the [Bazel][bazel] build system, using [rules_haskell][rules_haskell]
to define the Haskell build, including rules that wrap Cabal to build
third-party dependencies downloadable from Hackage.

[bazel]: https://bazel.build/
[rules_haskell]: https://haskell.build/

## Instructions

To build the package execute the following command *in the `examples/`
directory*:

```
$ bazel build //cat_hs/...
```

To run the tests execute the following command.

```
$ bazel test //cat_hs/...
```

To run the executable enter the following commands.

```
$ bazel run //cat_hs/exec/cat_hs -- -h
$ bazel run //cat_hs/exec/cat_hs -- $PWD/README.md
```
