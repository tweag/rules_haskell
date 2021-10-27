{-# LANGUAGE TemplateHaskell #-}
module BranchLeft(branch_left, root) where

import Language.Haskell.TH
import Root

branch_left :: Int
branch_left = $(runIO (print root) >> [| 3 * root |])
