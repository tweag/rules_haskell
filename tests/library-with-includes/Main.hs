module Main where

import Control.Monad (unless)
import Lib (x)

main :: IO ()
main = unless (x == (42 + 17)) (error "BAD VALUE")
