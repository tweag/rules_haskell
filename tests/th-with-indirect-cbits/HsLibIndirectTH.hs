{-# LANGUAGE TemplateHaskell #-}

module HsLibIndirectTH (one) where

import Language.Haskell.TH

one :: Q Exp
one = [|1|]
