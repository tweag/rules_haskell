module Main (main) where

import TestSubLib (messageEnd)
import Lib.Map

main :: IO ()
main = print $ Lib.Map.singleton 1 messageEnd
