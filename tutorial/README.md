# Tutorial code

Code for the [Bazel Haskell tutorial][bazel-haskell-tutorial].

[bazel-haskell-tutorial]: https://rules-haskell.readthedocs.io/en/latest/haskell.html

## **Important**

Run all commands from the root of `rules_haskell`.
If you `cd examples/`, bazel *will* [break on
you](https://github.com/tweag/rules_haskell/issues/740).
This is a current problem with bazel workspaces.

## Tutorial Workspace

Build everything in the tutorial workspace with;

```
$ bazel build @io_tweag_rules_haskell_tutorial//...
```

Show everything in the tutorial;

```
$ bazel query @io_tweag_rules_haskell_tutorial//...
@io_tweag_rules_haskell_tutorial//main:demorgan
@io_tweag_rules_haskell_tutorial//main:base
@io_tweag_rules_haskell_tutorial//lib:booleans
```

Build and run the tutorial example;

```
$ bazel build @io_tweag_rules_haskell_tutorial//lib:booleans
$ bazel build @io_tweag_rules_haskell_tutorial//main:demorgan
$ bazel run @io_tweag_rules_haskell_tutorial//main:demorgan
```
