This is a work-in-progress implementation of a persistent worker for
the `haskell_module` rule.

See [#1758](https://github.com/tweag/rules_haskell/issues/1758) to learn
about the standing issues to make it ready for general use.

To run `haskell_module` tests with it run
```
USE_BAZEL_VERSION=6.0.0-pre.20220520.1 bazelisk test \
  --experimental_worker_multiplex_sandboxing \
  --worker_max_instances=1 \
  --worker_quit_after_build \
  --define use_haskell_module_worker=True \
  --worker_verbose \
  --experimental_worker_max_multiplex_instances=2 \
  --worker_extra_flag --num-workers=2 \
  //tests/haskell_module/dep-narrowing-th/..
```

Bazel 6 is necessary to use the flag `--experimental_worker_multiplex_sandboxing`.

The flag `--worker_max_instances=1` tells `bazel` to spawn only one persistent
worker, but since this worker is multiplexed, it will spawn itself a number of
background workers that can compile modules concurrently.

The flag `--worker_quit_after_build` causes `bazel` to terminate persistent
workers at the end of a build. This is necessary to ensure that source code
doesn't change during the lifetime of a persistent worker, which simplifies
the handling of the internal worker state.

The flag `--define use_haskell_module_worker=True` advertises to `bazel` the
presence of a persistent worker for `haskell_module` rules. `bazel` will then
build the persistent worker and use it in the builds.

The flag `--experimental_worker_max_multiplex_instances=2` tells `bazel` to send
at most 2 concurrent requests to the persistent worker.

The flag `--worker_extra_flag --num-workers=2` tells the worker to spawn at most
two background workers. Each background worker is used to build the modules of a
library or binary, and it is discarded when requests to build new libraries or
binaries make it fall from an LRU cache of workers.

We have teams of background workers specialize on compiling modules of a
particular library and configuration to further simplify the management of the
internal state. The persistent worker implements an LRU cache to keep the amount
of background workers limited, and thus ensures that the memory does not fill up
with unused persistent workers.

For each compilation request we compute a key. Requests with the same key are
handled by background workers associated to this key in the LRU cache.

As a first attempt in the wild, the key is computed from the paths to the package
databases in the work request. This key is used to ensure that if the paths
change, we don't reuse the worker.

It would make sense to try eventually to make the key from a pair of the
library/binary name and the configuration hash. Both pieces of data are present
in the `-odir` flag, so we could use it to approximate the ideal key.
