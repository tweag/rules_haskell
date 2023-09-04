module Main (main) where

import TestLib (testMessage)
import qualified Test.Lib (testMessage)

main :: IO ()
main = do
  putStrLn testMessage
  putStrLn Test.Lib.testMessage
