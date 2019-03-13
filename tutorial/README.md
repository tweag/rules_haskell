# Tutorial code

Code for the [Bazel Haskell tutorial][bazel-haskell-tutorial].

[bazel-haskell-tutorial]: https://rules-haskell.readthedocs.io/en/latest/haskell.html

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
