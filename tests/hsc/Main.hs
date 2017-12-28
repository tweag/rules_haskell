module Main (main) where

import BinHsc ()
import Test (hscFired)

main :: IO ()
main = putStrLn hscFired
