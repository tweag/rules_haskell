module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import LibB(value)

main :: IO ()
main =
  unless (value == "42!") $ do
    hPutStrLn stderr "Expected 42!"
    exitFailure
