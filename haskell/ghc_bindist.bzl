"""Workspace rules (GHC binary distributions)"""

load("@bazel_skylib//:lib.bzl", "paths")

_GHC_BINS = {
    "8.4.1": {
        "linux-x86_64": ("https://downloads.haskell.org/~ghc/8.4.1/ghc-8.4.1-x86_64-deb8-linux.tar.xz", "427c77a934b30c3f1de992c38c072afb4323fe6fb30dbac919ca8cb6ae98fbd9"),
        "darwin-x86_64": ("https://downloads.haskell.org/~ghc/8.4.1/ghc-8.4.1-x86_64-apple-darwin.tar.xz", "d774e39f3a0105843efd06709b214ee332c30203e6c5902dd6ed45e36285f9b7"),
    },
    "8.2.2": {
        "linux-x86_64": ("https://downloads.haskell.org/~ghc/8.2.2/ghc-8.2.2-x86_64-deb8-linux.tar.xz", "48e205c62b9dc1ccf6739a4bc15a71e56dde2f891a9d786a1b115f0286111b2a"),
        "darwin-x86_64": ("https://downloads.haskell.org/~ghc/8.2.2/ghc-8.2.2-x86_64-apple-darwin.tar.xz", "f90fcf62f7e0936a6dfc3601cf663729bfe9bbf85097d2d75f0a16f8c2e95c27"),
    },
    "8.0.2": {
        "linux-x86_64": ("https://downloads.haskell.org/~ghc/8.0.2/ghc-8.0.2-x86_64-deb8-linux.tar.xz", "5ee68290db00ca0b79d57bc3a5bdce470de9ce9da0b098a7ce6c504605856c8f"),
        "darwin-x86_64": ("https://downloads.haskell.org/~ghc/8.0.2/ghc-8.0.2-x86_64-apple-darwin.tar.xz", "ff50a2df9f002f33b9f09717ebf5ec5a47906b9b65cc57b1f9849f8b2e06788d"),
    },
    "7.10.3": {
        "linux-x86_64": ("https://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-x86_64-deb8-linux.tar.xz", "b478e282afbf489614d0133ef698ba44e901eeb1794f4453c0fb0807cd271b96"),
        "darwin-x86_64": ("https://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-x86_64-apple-darwin.tar.xz", "852781d43d41cd55d02f818fe798bb4d1f7e52f488408167f413f7948cf1e7df"),
    },
}

_GHC_DEFAULT_VERSION = "8.4.1"

def _execute_fail_loudly(ctx, args):
    """Execute a command and fail loudly if it fails.

    Args:
      ctx: Repository rule context.
      args: Command and its arguments.
    """
    eresult = ctx.execute(args, quiet = False)
    if eresult.return_code != 0:
        fail("{0} failed, aborting creation of GHC bindist".format(" ".join(args)))

def _ghc_bindist_impl(ctx):
    # TODO adjust this when we support more platforms.
    if ctx.os.name[:5] == "linux":
        arch = "linux-x86_64"
    elif ctx.os.name == "mac os x":
        arch = "darwin-x86_64"
    else:
        fail("Operating system {0} is not yet supported.".format(ctx.os.name))

    version = ctx.attr.version
    if _GHC_BINS[version].get(arch) == None:
        fail("Operating system {0} does not have a bindist for GHC version {1}".format(ctx.os.name, ctx.attr.version))
    else:
        url, sha256 = _GHC_BINS[version][arch]

    bindist_dir = ctx.path(".")  # repo path

    ctx.download_and_extract(
        url = url,
        output = ".",
        sha256 = sha256,
        type = "tar.xz",
        stripPrefix = "ghc-" + version,
    )

    _execute_fail_loudly(ctx, ["./configure", "--prefix", bindist_dir.realpath])
    _execute_fail_loudly(ctx, ["make", "install"])

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
            default = _GHC_DEFAULT_VERSION,
            values = _GHC_BINS.keys(),
            doc = "The desired GHC version",
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
   load("@io_tweag_rules_haskell//haskell:haskell.bzl", "ghc_bindist")

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
