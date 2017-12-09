module Main (main) where

import Test (hscFired)

main :: IO ()
main = putStrLn hscFired
