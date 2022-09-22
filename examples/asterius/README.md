# asterius - An example on how to use 'asterius' with 'rules_haskell'

This project implements a very basic project with 2 modules.
It uses [Bazel][bazel] build system, using [rules_haskell][rules_haskell]
to define the Asterius Haskell build and output the webassmebly.

[bazel]: https://bazel.build/
[rules_haskell]: https://haskell.build/

## Instructions

To build the package execute the following command *in the `examples/asterius`
directory*:

```
$ bazel build //asterius:asterius_bundle
```
