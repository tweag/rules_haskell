module Main (main) where

import qualified Bazel.Runfiles as Runfiles
import Control.Monad (when)
import System.Info (os)
import System.Process (callProcess)

main :: IO ()
main = do
    r <- Runfiles.create
    foo <- readFile (Runfiles.rlocation r "rules_haskell/tools/runfiles/test-data.txt")
    when (lines foo /= ["foo"]) -- ignore trailing newline
        $ error $ "Incorrect contents: got: " ++ show foo
    let bin | os == "mingw32" =  "rules_haskell/tools/runfiles/bin.exe"
            | otherwise       =  "rules_haskell/tools/runfiles/bin"
    callProcess (Runfiles.rlocation r bin) []
