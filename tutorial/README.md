# Tutorial code

Code for the [Bazel Haskell tutorial][bazel-haskell-tutorial].

[bazel-haskell-tutorial]: https://rules-haskell.readthedocs.io/en/latest/haskell.html

## Tutorial Workspace

Build everything in the tutorial workspace with;

```
$ bazel build //...
```

Show everything in the tutorial;

```
$ bazel query //...
//main:demorgan
//main:base
//lib:booleans
```

Build and run the tutorial example;

```
$ bazel build //lib:booleans
$ bazel build //main:demorgan
$ bazel run //main:demorgan
```
