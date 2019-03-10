{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Bool
import qualified Prelude
import Prelude ((++), (==), ($))

deriving instance Prelude.Eq Bool

bools :: [Bool]
bools = [False, True]

main =
    Prelude.print $ Prelude.and $
      [ not (x `and` y) == not x `or` not y | x <- bools, y <- bools]
