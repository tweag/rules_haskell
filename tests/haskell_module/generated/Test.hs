module Main where

import Control.Monad (unless)
import qualified Module.A
import qualified Module.B
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  let addedValue = Module.A.value + Module.B.value
  unless (addedValue == 42) $ do
    hPutStrLn stderr $ "Expected 42 but got " ++ show addedValue
    exitFailure
