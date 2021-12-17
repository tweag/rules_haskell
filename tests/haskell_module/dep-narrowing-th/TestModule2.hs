{-# LANGUAGE TemplateHaskell #-}
module TestModule2 where

import Control.Exception (evaluate)
import Language.Haskell.TH
import SimpleFoo

runIO (evaluate foo) >> return []

f :: IO ()
f = return ()
