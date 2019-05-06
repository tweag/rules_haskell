module Main (main) where

import Control.Monad (unless)
import Lib (three)

main :: IO ()
main =
    unless (three == 3) $
        fail "Unexpected values import from C libraries."
