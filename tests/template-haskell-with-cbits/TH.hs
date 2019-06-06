{-# LANGUAGE TemplateHaskell #-}

module TH where

import Language.Haskell.TH

foreign import ccall "c_add_one" c_add_one' :: Int -> Int

value :: Q Exp
value = [| c_add_one' 41 |]
