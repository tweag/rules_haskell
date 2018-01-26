module Main (main) where

import BinHsc ()
import Test0 (hscFired0)
import Test1 (hscFired1)

main :: IO ()
main = putStrLn (hscFired0 ++ hscFired1)
