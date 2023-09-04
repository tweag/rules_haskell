module Main where

import Control.Monad (unless)
import Lib           (value)

main = unless (value == 42)
    $ error $ "Incorrect lib value. Got " <> show value
