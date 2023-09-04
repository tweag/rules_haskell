{-# LANGUAGE MagicHash #-}

module Main where
import GHC.Integer.Logarithms
import GHC.Exts
import Text.XHtml

main = do
  print $ I# (integerLog2# 8)
  print $ head treeColors
