module Main where

import Compile
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  print args
  st <- runSession $ compile args 0
  print st
