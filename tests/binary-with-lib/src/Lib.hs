{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib (value) where

import Language.Haskell.TH

value :: Q Exp
value = [|42|]
