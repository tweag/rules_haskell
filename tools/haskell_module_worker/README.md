This is a work-in-progress implementation of a persistent worker for
the `haskell_module` rule.

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
two backgound compilation servers. Each compilation server is used to build the
modules of a library or binary, and it is discarded when requests to build new
libraries or binaries make it fall from an LRU cache of workers.

We have teams of background workers specialize on compiling modules of a
particular library to further simplify the management of the internal state.
Once persistent workers are specialized to libraries, `bazel` is unable to
constrain the total amount of workers. `bazel` can only constrain the total
amount of workers assigned to a combination of mnemonic and startup parameters.
Or said differently it can only constrain the amount of workers assigned to a
particular library. This is why this persistent worker implements an LRU cache
to keep the amount of workers limited, and thus ensures that the memory does
not fill up with unused persistent workers.

A few issues need to be investigated to make this implementation ready for
general use. At the time of this writting they are:

* Multiplex sandboxing in Bazel 6 does not reliably expose all of the inputs
  to the persistent worker yet.
* The persistent worker sometimes fails when attempting to reload libraries
  for TH that have been already loaded.
* The persistent worker fails sometimes when interface files provided to
  earlier compilation requests are not available to newer compilation requests.
  This can happen if different modules depend on a same library, but each of
  them uses different modules from this library.
