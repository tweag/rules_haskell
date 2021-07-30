module Lib where
import Paths_lib_pkg
import Control.Exception
import System.Exit
import Data.List

readDataFile :: IO ()
readDataFile = do
  p <- getDataFileName "datafile"
  handle ((\e -> print p >> (exitWith $ ExitFailure 1)):: IOException -> IO ()) $ do
      s <- readFile p
      return ()

