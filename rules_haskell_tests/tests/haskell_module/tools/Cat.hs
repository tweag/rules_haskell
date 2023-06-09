module Main where

import System.Environment (getArgs)

main = do
  [name, src, dest] <- getArgs
  contents <- readFile src
  writeFile dest contents
