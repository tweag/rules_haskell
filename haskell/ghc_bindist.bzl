"""Workspace rules (GHC binary distributions)"""

load("@bazel_skylib//:lib.bzl", "paths")

_GHC_BINS = {
  "8.2.2": {
    "linux-x86_64": ("https://downloads.haskell.org/~ghc/8.2.2/ghc-8.2.2-x86_64-deb8-linux.tar.xz", "48e205c62b9dc1ccf6739a4bc15a71e56dde2f891a9d786a1b115f0286111b2a"),
  },
  "8.0.2": {
    "linux-x86_64": ("https://downloads.haskell.org/~ghc/8.0.2/ghc-8.0.2-x86_64-deb8-linux.tar.xz", "5ee68290db00ca0b79d57bc3a5bdce470de9ce9da0b098a7ce6c504605856c8f"),
  },
  "7.10.3": {
    "linux-x86_64": ("https://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-x86_64-deb8-linux.tar.xz", "b478e282afbf489614d0133ef698ba44e901eeb1794f4453c0fb0807cd271b96"),
  },
}

_GHC_DEFAULT_VERSION = "8.2.2"

def _ghc_bindist_impl(ctx):

  if ctx.os.name[:5] != "linux":
    fail("Operating system {0} is not yet supported.".format(ctx.os.name))

  version = ctx.attr.version
  arch = "linux-x86_64" # TODO Adjust this when we support more platforms.
  url, sha256 = _GHC_BINS[version][arch]

  bindist_dir = ctx.path(".") # repo path

  ctx.download_and_extract(
    url         = url,
    output      = ".",
    sha256      = sha256,
    type        = "tar.xz",
    stripPrefix = "ghc-" + version,
  )

  ctx.execute(["./configure", "--prefix", bindist_dir.realpath])
  ctx.execute(["make", "install"])

  ctx.template(
    "BUILD",
    Label("//haskell:ghc.BUILD"),
    executable = False,
  )

ghc_bindist = repository_rule(
  _ghc_bindist_impl,
  local = False,
  attrs = {
    "version": attr.string(
      default=_GHC_DEFAULT_VERSION,
      values=_GHC_BINS.keys(),
      doc = "The desired GHC version"
    ),
  },
)
"""Create a new repository with given `name` which will contain two
targets:

* filegroup `bin` containing all executable files of GHC
* C library `threaded-rts`

Example:
   In `WORKSPACE` file:

   ```bzl
   # This repository rule creates @ghc repository.
   ghc_bindist(
     name    = "ghc",
     version = "8.2.2",
   )

   # Register the toolchain defined locally in BUILD file:
   register_toolchain("//:ghc")
   ```

   In `BUILD` file:

   ```bzl
   # Use binaries from @ghc//:bin to define //:ghc toolchain.
   haskell_toolchain(
     name = "ghc",
     version = "8.2.2",
     tools = "@ghc//:bin",
   )
   ```
"""
