module Main (main) where

import Control.Monad (unless)
import LibWithData (readDataFile)
import System.Exit (exitFailure)

main :: IO ()
main = do
  content <- readDataFile
  unless (content == "datafile_content\n") $ do
    putStrLn ("unexpected datafile content: " ++ show content)
    exitFailure
