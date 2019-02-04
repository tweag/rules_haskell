{-# LANGUAGE TemplateHaskell #-}

module HsLibDirect where

import HsLibIndirect
import Language.Haskell.TH

direct :: Q Exp
direct = [|indirect 1|]
