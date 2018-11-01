module Main (main) where

import qualified Bazel.Runfiles as Runfiles
import Control.Monad (when)
import System.FilePath ((</>))

main :: IO ()
main = do
    r <- Runfiles.create
    bar <- readFile (Runfiles.rlocation r "io_tweag_rules_haskell/tools/runfiles/bin-data.txt")
    when (lines bar /= ["bar"]) -- ignore trailing newline
        $ error $ "Incorrect contents: got: " ++ show bar
