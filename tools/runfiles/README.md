# Bazel Runfiles

This is a small utility to enable discovery of the Bazel runfiles location. This is useful in tests for example.

```haskell
module Main (main) where

import qualified Bazel.Runfiles as Runfiles
import Control.Monad (when)
import System.Process (callProcess)

main :: IO ()
main = do
    r <- Runfiles.create
    foo <- readFile (Runfiles.rlocation r "rules_haskell/tools/runfiles/test-data.txt")
    when (lines foo /= ["foo"]) -- ignore trailing newline
        $ error $ "Incorrect contents: got: " ++ show foo
    callProcess (Runfiles.rlocation r "rules_haskell/tools/runfiles/bin") []
```

## `haskell_runfiles`

To resolve repository mappings with bzlmod, the runfiles library must know the name of the bazel module calling it 
(see the [design document](https://github.com/bazelbuild/proposals/blob/main/designs/2022-07-21-locating-runfiles-with-bzlmod.md#3-make-the-canonical-repository-name-of-the-current-repository-available-at-runtime)).

This is achieved by passing the `__FILE__` constant to the `createFromCurrentFile` function of the library.

Since enabling the preprocessor can be inconvenient in some cases, the [haskell_runfiles](https://api.haskell.build/haskell/defs.html#haskell_runfiles) helper rule is provided in `@rules_haskell//haskell:defs.bzl`,
which generates a small wrapper around the main `runfiles` library.

Note: using the `haskell_runfiles` rule is only required when the library accessing runfiles is used from another bazel module (which uses a different repository mapping).
If the binary is defined in the same module, the default behavior which recovers the current module from the path of the binary is correct.
