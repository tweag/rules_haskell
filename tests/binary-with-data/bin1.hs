{-# LANGUAGE CPP #-}

module Main where

import qualified Bazel.Runfiles
import Control.Monad (unless)

main :: IO ()
main = do
    runfiles <- Bazel.Runfiles.create
    let path = Bazel.Runfiles.rlocation runfiles ("rules_haskell/" ++ BIN1_INPUT)
    contents <- readFile path
    unless (contents == "contents\n")
      $ error $ "Incorrect input; got " ++ show contents
