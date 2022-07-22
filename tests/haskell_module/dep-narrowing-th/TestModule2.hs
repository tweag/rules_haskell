{-# LANGUAGE TemplateHaskell #-}
module TestModule2 where

import Language.Haskell.TH
import SimpleFoo

runIO foo >> return []

f :: IO ()
f = return ()
