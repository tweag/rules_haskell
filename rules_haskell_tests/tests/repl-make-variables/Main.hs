{-# LANGUAGE CPP #-}

module Main where

import qualified Bazel.Runfiles
import Control.Monad (unless)
import System.FilePath ((</>))

-- haskell_repl fails to build on compiler errors of its `deps` when
-- `collect_data = True`. In this case we need `collect_data = True`
-- because of the `data.txt` dependency. The following CPP macro avoids
-- the compiler error. See
-- https://github.com/tweag/rules_haskell/issues/1380
#ifndef DATA
#define DATA "0"
#endif

main :: IO ()
main = do
  runfiles <- Bazel.Runfiles.create
  content <- readFile (Bazel.Runfiles.rlocation runfiles $ "rules_haskell_tests" </> DATA)
  unless (content == "42\n") $
    fail "Unexpected content"
