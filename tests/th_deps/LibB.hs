{-# LANGUAGE TemplateHaskell #-}
module LibB(value) where

import Language.Haskell.TH
import TH(valueQ)

value :: String
value = $valueQ
