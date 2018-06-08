module Main (main) where

import Bazel.Runfiles
import Control.Monad (when)
import System.FilePath ((</>))
import System.Process (callProcess)


main :: IO ()
main = do
    r <- getRunfiles
    foo <- readFile (rlocation r "io_tweag_rules_haskell/tools/runfiles/test-data.txt")
    when (lines foo /= ["foo"]) -- ignore trailing newline
        $ error $ "Incorrect contents: got: " ++ show foo
    callProcess (rlocation r "io_tweag_rules_haskell/tools/runfiles/bin") []
