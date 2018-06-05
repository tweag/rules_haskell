{-# LANGUAGE TemplateHaskell #-}

module FooTH (embedFile) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

embedFile :: FilePath -> Q Exp
embedFile path = do
  str <- runIO (readFile path)
  addDependentFile path
  [| str |]
