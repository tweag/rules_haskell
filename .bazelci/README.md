# Continuous integration of the upstream bazel project (Google)

This directory is the config steering our upstream CI pipeline, [which
you can find here](https://buildkite.com/bazel/rules-haskell-haskell).

[@mboes](https://github.com/mboes), [@aherrmann](https://github.com/aherrmann) and [@Profpatsch](https://github.com/Profpatsch) have *rule maintainer* access.
They can verify pull requests to be built on the buildkite CI
machines.

The bazel team will use this integration to make sure that new
(compatible) bazel releases do not break `rules_haskell`.

The bazel buildkite continuous integration is described in [this document](https://github.com/bazelbuild/continuous-integration/blob/master/buildkite/README.md).

