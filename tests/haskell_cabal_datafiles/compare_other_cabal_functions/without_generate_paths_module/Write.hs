module Main where

import Lib
import System.Environment

main = do
  [outputFileName] <- getArgs
  writeCabalPathsValues outputFileName

