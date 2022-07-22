module Main where

import LibA (double)

main :: IO ()
main = do
  putStrLn $ show (double 21)
