{-# LANGUAGE ForeignFunctionInterface #-}
module Lib (ten) where

import Foreign.C.Types (CInt(..))

foreign import ccall "c_add_one"
  c_add_one :: CInt -> CInt

ten :: Int
ten = fromIntegral (c_add_one 9)
