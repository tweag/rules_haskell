module Main where

import qualified HscLib
import qualified HsLib

main :: IO ()
main = do
  HscLib.check
  HsLib.check
