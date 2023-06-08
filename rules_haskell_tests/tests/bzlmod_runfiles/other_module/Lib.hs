{-# LANGUAGE CPP #-}

module Lib where

import qualified Bazel.Runfiles as Runfiles
import Control.Monad (unless)
import System.Environment (getArgs, getEnv, unsetEnv, getEnvironment)
import System.IO (IOMode (ReadMode), hGetLine, withFile)

content :: IO String
content = do
  r <- Runfiles.create
  -- r <- Runfiles.createFromCurrentFile __FILE__
  let location = Runfiles.rlocation r "other_module/datafile"
  withFile location ReadMode hGetLine
