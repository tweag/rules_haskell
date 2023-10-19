module Main (main) where

import TestSubLib (messageEnd)

main :: IO ()
main = putStrLn $ messageEnd
