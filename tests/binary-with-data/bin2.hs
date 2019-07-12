module Main where

import qualified Bazel.Runfiles
import System.Process (callProcess)
import System.Environment (getArgs)

main = do
  [arg] <- getArgs
  runfiles <- Bazel.Runfiles.create
  let path = Bazel.Runfiles.rlocation runfiles ("io_tweag_rules_haskell/" ++ arg)
  callProcess path []
