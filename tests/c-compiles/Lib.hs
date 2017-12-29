{-# LANGUAGE ForeignFunctionInterface #-}
module Lib (ten) where

import Foreign.C.Types (CInt(..))

foreign import ccall "add_five"
  c_add_five :: CInt -> CInt

ten :: Int
ten = fromIntegral (c_add_five 5)
