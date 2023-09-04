{-# LANGUAGE TemplateHaskell #-}
module Leaf where

import BranchLeft
import BranchRight
import Control.Monad (replicateM)
import qualified Data.Vector as Vector
import Language.Haskell.TH (runIO)

runIO (replicateM root (return Vector.empty)) >> return []

leaf :: Int
leaf = 7 * branch_left * branch_right
