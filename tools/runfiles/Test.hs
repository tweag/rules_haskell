module Main (main) where

import qualified Bazel.Runfiles as Runfiles
import Control.Monad (when)
import System.Process (callProcess)

main :: IO ()
main = do
    r <- Runfiles.create
    foo <- readFile (Runfiles.rlocation r "io_tweag_rules_haskell/tools/runfiles/test-data.txt")
    when (lines foo /= ["foo"]) -- ignore trailing newline
        $ error $ "Incorrect contents: got: " ++ show foo
    callProcess (Runfiles.rlocation r "io_tweag_rules_haskell/tools/runfiles/bin") []
