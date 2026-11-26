module Main (main) where

import Lib (readDataFile)
import System.Exit (exitFailure)

main :: IO ()
main = do
  content <- readDataFile
  if content == "reloc-lib data\n"
    then putStrLn "OK"
    else putStrLn ("Unexpected datafile content: " ++ show content) >> exitFailure
