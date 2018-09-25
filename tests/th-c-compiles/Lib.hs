{-# LANGUAGE ForeignFunctionInterface #-}
module Lib (tenLit) where

import Foreign.C.Types (CInt(..))
import Language.Haskell.TH.Syntax


foreign import ccall "add_five"
  c_add_five :: CInt -> CInt

tenLit :: Q Exp
tenLit = pure (LitE (IntegerL (fromIntegral (c_add_five 5))))
