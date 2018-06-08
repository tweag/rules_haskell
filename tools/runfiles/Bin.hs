module Main (main) where

import Bazel.Runfiles
import Control.Monad (when)
import System.FilePath ((</>))

main :: IO ()
main = do
    r <- getRunfiles
    bar <- readFile (rlocation r "io_tweag_rules_haskell/tools/runfiles/bin-data.txt")
    when (lines bar /= ["bar"]) -- ignore trailing newline
        $ error $ "Incorrect contents: got: " ++ show bar
