module Main (main) where

import TestSubLib (messageEnd)
import Lib.List

main :: IO ()
main = putStrLn $ Lib.List.tail messageEnd
