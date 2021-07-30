module Main where

import Paths_generate_paths_module_binary
import Control.Exception
import System.Exit

readDataFile :: IO ()
readDataFile = do
  p <- getDataFileName "datafile"
  handle ((\e -> print p >> (exitWith $ ExitFailure 1)):: IOException -> IO ()) $ do
      s <- readFile p
      return ()

main = readDataFile
