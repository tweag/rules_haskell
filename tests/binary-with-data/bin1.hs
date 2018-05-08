module Main where

import Control.Monad (unless)

main :: IO ()
main = do
    contents <- readFile "tests/binary-with-data/bin1-input"
    unless (contents == "contents\n")
      $ error $ "Incorrect input; got " ++ show contents
