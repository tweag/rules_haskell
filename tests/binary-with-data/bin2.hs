module Main where

import System.Process (callProcess)
import System.Environment (getArgs)

main = do
  [arg] <- getArgs
  callProcess arg []
