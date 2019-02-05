{-# LANGUAGE TemplateHaskell #-}

module HsLibIndirectTH (one) where

import Language.Haskell.TH

foreign import ccall "direct" c_direct :: Int -> Int

one :: Q Exp
one = [|c_direct 0|]
