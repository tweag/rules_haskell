{-# LANGUAGE TemplateHaskell #-}

module LibA where

import Language.Haskell.TH

value :: Q Exp
value = [| 42 |]
