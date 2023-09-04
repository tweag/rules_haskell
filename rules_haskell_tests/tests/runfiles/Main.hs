module Main where

import qualified Bazel.Runfiles as Runfiles
import Control.Monad (unless)
import System.Environment (getArgs, getEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode), hGetLine, withFile)

main :: IO ()
main = do
  workspace <- getEnv "TEST_WORKSPACE"
  [expected, filename] <- getArgs
  r <- Runfiles.create
  let location = Runfiles.rlocation r (workspace </> filename)
  content <- withFile location ReadMode hGetLine
  unless (content == expected) $
    fail $ "Expected '" ++ expected ++ "' but found '" ++ content ++ "'"
