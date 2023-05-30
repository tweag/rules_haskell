module Main (main) where

import Control.Monad (unless)
import Lib           (ten)

main :: IO ()
main = unless (ten == 10)
    $ error $ "Incorrect lib value. Got " <> show ten
