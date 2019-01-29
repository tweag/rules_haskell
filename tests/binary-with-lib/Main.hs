{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (unless)
import Lib           (value)
import Language.Haskell.TH

val = $(value)

main = unless (val == 42)
    $ error $ "Incorrect lib value. Got " <> show val
